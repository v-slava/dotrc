#!/bin/bash

source ~/.config_xdg/BT_MAC.sh

# BLUETOOTH_CONNECTED=$DOTRC/other_files/bluetooth_connected.sh
# if $BLUETOOTH_CONNECTED ; then # connected => need to disconnect
# else # disconnected => need to connect
# fi

set -e
ACTION="$1"
if [ -z "$ACTION" ]; then
    echo -e "Type action number:\n\n1: connect\n2: disconnect\n\nYour choice: "
    read -n1 ACTION_NUMBER
    case $ACTION_NUMBER in
        "1") ACTION=connect ;;
        "2") ACTION=disconnect ;;
        *) echo "Wrong action selected" 1>&2 ; exit 1 ;;
    esac
fi

echo "$ACTION $BT_MAC" | bluetoothctl
journalctl --follow _SYSTEMD_UNIT=bluetooth.service
