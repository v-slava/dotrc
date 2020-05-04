#!/bin/bash

$DOTRC/other_files/xrandr.sh xinitrc

$DOTRC/other_files/lock_screen.sh &

# invoke global X session script
. /etc/X11/Xsession
