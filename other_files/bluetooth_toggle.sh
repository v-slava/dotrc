#!/bin/bash

# Toggles bluetooth connection status:
# If was connected    -> disconnecting
# If was disconnected -> connecting

source ~/.config_xdg/BT_MAC.sh

BLUETOOTH_CONNECTED=~/os_settings/other_files/bluetooth_connected.sh

if $BLUETOOTH_CONNECTED ; then # connected => need to disconnect

	cat << EOF | expect 2>&1
set timeout 10
spawn bluetoothctl
expect "\[bluetooth\]"
expect "\# "
send "disconnect $BT_MAC\n"
expect "Attempting to disconnect from"
set RET 0
expect {
	"Successful disconnected" { set RET 1 }
	timeout { exit 3 }
}
expect "\[bluetooth\]"
expect "\# "
send "quit\n"
exit \$RET
EOF
	bluetoothctl_RET=$?
	echo -e "\n"
	if [ $bluetoothctl_RET -ne 1 ]; then
		echo "Failed to disconnect from bluetooth headset $BT_MAC." 1>&2
		exit 1
	fi
	~/os_settings/other_files/set_volume.sh 20%
	echo "Disconnected from bluetooth headset $BT_MAC."

else # disconnected => need to connect

	bluetoothctl_RET=0
	while [ $bluetoothctl_RET -ne 1 ]; do
		cat << EOF | expect 2>&1
set timeout 10
spawn bluetoothctl
expect "\[bluetooth\]"
expect "\# "
send "connect $BT_MAC\n"
expect "Attempting to connect to"
set RET 0
expect {
	"Connection successful" { set RET 1 }
	"Connected: yes" { set RET 1 }
	"Connected: no" { set RET 2 }
	"Failed to connect" { set RET 2 }
	timeout { exit 3 }
}
expect "\[bluetooth\]"
expect "\# "
send "quit\n"
exit \$RET
EOF
		bluetoothctl_RET=$?
		echo -e "\n"
		if [ $bluetoothctl_RET -ne 1 ]; then
			echo -e "Failed to connect to bluetooth headset $BT_MAC.\n" 1>&2
		fi
	done # while [ $bluetoothctl_RET -ne 1 ]
	# Set bluetooth profile
	CARD_NAME=$(~/os_settings/other_files/get_card_name.sh bluez_card)
	RET=1
	set -x
	while [ $RET -ne 0 ]; do
		pactl set-card-profile "$CARD_NAME" a2dp_sink
		RET=$?
	done
	set -e
	echo "Attempting to get full sink name for pattern bluez_sink.* ..."
	BT_SINK="$(~/os_settings/other_files/get_sink_name.sh bluez_sink)"
	pacmd set-default-sink "$BT_SINK"
	~/os_settings/other_files/set_volume.sh 20%
	echo "Connected to bluetooth headset $(hcitool name $BT_MAC) (MAC = $BT_MAC)."
fi

