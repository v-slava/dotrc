#!/bin/bash

set -e

setxkbmap us
# Turn off Caps_Lock if needed:
if xset q | grep -q 'Caps Lock:   on' ; then
	xdotool key Caps_Lock
fi
xmodmap -e 'remove Lock = Caps_Lock' -e 'keycode 66 = Control_L'
i3lock -c 101010 --nofork
setxkbmap us,ru
~/os_settings/other_files/keyboard_layout.sh

