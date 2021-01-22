#!/bin/bash

set -e

if [ "$1" = "--spawn-terminal" ]; then
    # See: $DOTRC/other_files/open_terminal.sh
    source ~/.bashrc
    exec x-terminal-emulator -title "Toggle audio device" -geometry 120x30 \
        -e bash -c "$DOTRC/other_files/audio_device_toggle.sh || vifm-pause"
fi

# ACTION="$1"
# if [ -z "$ACTION" ]; then
#     echo -e "Type action number:\n\n1: connect\n2: disconnect\n\nYour choice: "
#     read -n1 ACTION_NUMBER
#     case $ACTION_NUMBER in
#         "1") ACTION=connect ;;
#         "2") ACTION=disconnect ;;
#         *) echo "Wrong action selected" 1>&2 ; exit 1 ;;
#     esac
# fi
# echo -e "\n"

if $DOTRC/other_files/bluetooth_connected.sh ; then
    ACTION=disconnect
else
    ACTION=connect
fi

if [ -n "$BT_MAC" ]; then
    echo "$ACTION $BT_MAC" | bluetoothctl
    journalctl --follow _SYSTEMD_UNIT=bluetooth.service
else
    # Assume this is not a bluetooth but rather a USB headset.
    case $ACTION in
        "connect")
            SINK=$(LANG=C pactl list sinks | grep 'Name: ' | cut -d' ' -f2 | grep usb)
            SOURCE=$(LANG=C pactl list sources  | grep 'Name: ' | cut -d' ' -f2 | grep -v monitor | grep usb)
            ;;
        "disconnect")
            SINK=$(LANG=C pactl list sinks | grep 'Name: ' | cut -d' ' -f2 | grep -v usb)
            SOURCE=$(LANG=C pactl list sources  | grep 'Name: ' | cut -d' ' -f2 | grep -v monitor | grep -v usb)
            ;;
        *) echo "Wrong action selected" 1>&2 ; exit 1 ;;
    esac
    echo -e "\n"
    set -x
    pactl set-default-sink "$SINK"
    pactl set-default-source "$SOURCE"
    $DOTRC/other_files/use_default_sink_source.sh
    set +x
    echo -e "\nSuccess!"
    vifm-pause
fi
