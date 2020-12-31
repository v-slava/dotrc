#!/bin/bash

$DOTRC/other_files/lock_screen.sh &

$DOTRC/other_files/xrandr.sh xinitrc

# Do not turn screen off:
xset s off -dpms

# invoke global X session script
. /etc/X11/Xsession
