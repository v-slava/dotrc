#!/bin/bash

# for_window [class="URxvt" title="suspend script"] floating enable
# bindsym $Ctrl_L+$Alt_L+s exec --no-startup-id exec $START $DOTRC/other_files/suspend_hotkey.sh

set -e

$DOTRC/other_files/lock_screen.sh &
x-terminal-emulator -title "suspend script" -e bash -c \
'$DOTRC/other_files/update_system.sh && exec sudo systemctl suspend'
