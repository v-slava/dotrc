#!/bin/bash

# Toggles bluetooth connection status:
# If was connected    -> disconnecting
# If was disconnected -> connecting

source ~/.config_xdg/BT_MAC.sh

BLUETOOTH_CONNECTED=$DOTRC/other_files/bluetooth_connected.sh

if $BLUETOOTH_CONNECTED ; then # connected => need to disconnect

    set -e
    SPEAKERS_SINK="$($DOTRC/other_files/get_sink_name.sh alsa_output)"
    pactl -- set-sink-mute "$SPEAKERS_SINK" 1
    set +e
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
    set -e
    $DOTRC/other_files/set_volume.sh 20%
    pactl -- set-sink-mute "$SPEAKERS_SINK" 0
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
    "Failed to connect: org.bluez.Error.NotReady" { set RET 3 }
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
        if [ $bluetoothctl_RET -eq 3 ]; then
            echo "Toggling bluetooth USB port has been disabled..."
            # sudo $DOTRC/other_files/toggle_bluetooth_usb_port.sh
            sleep 1
        fi
        if [ $bluetoothctl_RET -ne 1 ]; then
            echo -e "Failed to connect to bluetooth headset $BT_MAC.\n" 1>&2
        fi
    done # while [ $bluetoothctl_RET -ne 1 ]
    set -e
    echo "Attempting to get full sink name for pattern bluez_sink.* ..."
    BT_SINK="$($DOTRC/other_files/get_sink_name.sh bluez_sink)"
    echo "Got full sink name: |$BT_SINK|"
    pactl -- set-sink-mute "$BT_SINK" 1
    echo "$BT_SINK muted"
    # Set bluetooth profile
    CARD_NAME=$($DOTRC/other_files/get_card_name.sh bluez_card)
    echo "Got CARD_NAME = |$CARD_NAME|"
    set +e
    RET=1
    while [ $RET -ne 0 ]; do
        # TODO:
        # a2dp_sink: High Fidelity Playback (A2DP Sink) (sinks: 1, sources: 0, priority: 10, available: no)
        # headset_head_unit: Headset Head Unit (HSP/HFP) (sinks: 1, sources: 1, priority: 20, available: yes)
        pactl set-card-profile "$CARD_NAME" a2dp_sink 2>/dev/null
        RET=$?
    done
    echo "Set card profile: |$CARD_PROFILE|"
    set -e
    pacmd set-default-sink "$BT_SINK"
    $DOTRC/other_files/set_volume.sh 20%
    pactl -- set-sink-mute "$BT_SINK" 0
    # Move application producing sound to bluetooth headset:
    SINK_INPUT_INDICES=$(pacmd list-sink-inputs | grep 'index: ' | cut -d':' -f2 | cut -d' ' -f2)
    for sink_input_index in $SINK_INPUT_INDICES ; do
        pacmd move-sink-input $sink_input_index $BT_SINK
    done
    echo "Connected to bluetooth headset $(hcitool name $BT_MAC) (MAC = $BT_MAC)."
fi
