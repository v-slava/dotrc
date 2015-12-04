#!/bin/bash

set -e

setxkbmap us
xmodmap -e 'remove Lock = Caps_Lock' -e 'keycode 9 = Control_L'
i3lock -c 101010 --nofork
setxkbmap us,ru
~/os_settings/other_files/keyboard_layout.sh

