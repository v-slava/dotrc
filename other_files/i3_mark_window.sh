#!/bin/bash

MARK=$(dmenu -fn 'Inconsolata LGC-16:monospace' -p 'Mark current window as:' < /dev/null)

# echo $MARK
i3-msg "mark --replace $MARK"
