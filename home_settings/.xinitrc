#!/bin/bash

$DOTRC/other_files/lock_screen.sh &

$DOTRC/other_files/xrandr.sh xinitrc

# invoke global X session script
. /etc/X11/Xsession
