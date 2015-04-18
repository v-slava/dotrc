#!/bin/bash

# Toggles bluetooth connection status:
# If was connected    -> disconnecting
# If was disconnected -> connecting

set -e

BLUETOOTH_CONNECTED=~/os_settings/other_files/bluetooth_connected.sh

if $BLUETOOTH_CONNECTED ; then
	echo "disconnect $BT_MAC" | bluetoothctl
	sleep 1
	while ! $BLUETOOTH_CONNECTED ; do
		sleep 1
	done
	zenity --info --title "bluetooth headset" --text "Disconnected."
else
	echo "connect $BT_MAC" | bluetoothctl
	sleep 1
	BT_SINK=$(pacmd list-sinks | grep -o "bluez_sink\.$(echo $BT_MAC | sed 's/:/\./g')")
	while [ -z "$BT_SINK" ]; do
		sleep 1
		BT_SINK=$(pacmd list-sinks | grep -o "bluez_sink\.$(echo $BT_MAC | sed 's/:/\./g')")
	done
	pacmd set-default-sink $BT_SINK
	zenity --info --title "Bluetooth headset" --text "Connected to $(hcitool name $BT_MAC) (MAC = $BT_MAC)."
fi

