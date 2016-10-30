#!/bin/bash

set -e

setxkbmap us
# Turn off Caps_Lock if needed:
if xset q | grep -q 'Caps Lock:   on' ; then
	xdotool key Caps_Lock
fi
xmodmap ~/.Xmodmap
set +e # since Caps_Lock may be undefined, the following command may fail
xmodmap -e 'remove Lock = Caps_Lock' -e 'keysym Caps_Lock = BackSpace'
set -e

i3lock -c 101010 --nofork

setxkbmap us,ru
xmodmap ~/.Xmodmap
# xcape -e 'Control_L=Escape' # Remap <Caps_Lock> => <Escape>
if ! pgrep fbxkb ; then
	fbxkb &
fi

