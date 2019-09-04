#!/bin/bash

set -e

if ! $DOTRC/other_files/virtual_box.sh ; then
    $DOTRC/other_files/lock_screen.sh &
fi

x-terminal-emulator -title "hibernate script" -e bash -c \
    '$DOTRC/other_files/update_system.sh && exec sudo poweroff'
# sudo systemctl hibernate
