#!/bin/bash

FILE=/media/files/autogenerated/do_not_lock_screen

if [ "$1" = "sway_hotkey" ]; then
    if [ -e "$FILE" ]; then
        mv "$FILE" "${FILE}1"
    fi
fi

if [ -e "$FILE" ]; then
    DO_LOCK=false
else
    DO_LOCK=true
fi

if [ "$DO_LOCK" = "false" ]; then
    exit
fi

set -e

# For i3wm:
# setxkbmap us
# # Turn off Caps_Lock if needed:
# if xset q | grep -q 'Caps Lock:   on' ; then
	# xdotool key Caps_Lock
# fi
# xmodmap ~/.Xmodmap
# set +e # since Caps_Lock may be undefined, the following command may fail
# xmodmap -e 'remove Lock = Caps_Lock' -e 'keysym Caps_Lock = BackSpace' 2>/dev/null
# set -e

# For sway:
$DOTRC/other_files/keyboard_layout.sh US

$DOTRC/other_files/screen_brightness.sh --off
# i3lock -c 101010 --nofork
swaylock -c 101010

# For i3wm:
# setxkbmap us,ru
# $DOTRC/other_files/init_keyboard_layout.sh
