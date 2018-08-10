#!/bin/bash

source ~/.bashrc
BLUETOOTH_TOGGLE_SH=$DOTRC/other_files/bluetooth_toggle.sh

x-terminal-emulator -title "My bluetooth headset" -geometry 70x13 -e bash -c \
    "$BLUETOOTH_TOGGLE_SH || vifm-pause"
