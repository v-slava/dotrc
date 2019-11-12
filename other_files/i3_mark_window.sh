#!/bin/bash

MARK=$(dmenu -fn 'Inconsolata LGC-16:monospace' -p 'Mark current window as:' < /dev/null)

# echo $MARK
source $DOTRC/other_files/i3_msg.sh
i3_msg "mark --replace $MARK"
