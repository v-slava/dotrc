#!/bin/bash

# For sway:
MARK=$(dmenu -p 'Mark current window as:' < /dev/null)
exec swaymsg "mark --replace $MARK"

# For i3wm:
MARK=$(dmenu -fn 'Inconsolata LGC-16:monospace' -p 'Mark current window as:' < /dev/null)
source $DOTRC/other_files/i3_msg.sh
exec i3_msg "mark --replace $MARK"
