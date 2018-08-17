#!/bin/bash

set -e

if $DOTRC/other_files/virtual_box.sh ; then
    sudo poweroff
else
    $DOTRC/other_files/lock_screen.sh &
    x-terminal-emulator -title "hibernate script" -e bash -c \
'source ~/.bashrc && $DOTRC/other_files/update_system.sh && exec sudo poweroff'
    # sudo systemctl hibernate
fi
