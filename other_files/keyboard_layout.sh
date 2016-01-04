#!/bin/bash

# killall xcape

set -e
xmodmap ~/.Xmodmap
# xcape -e 'Control_L=Escape' # Remap <Caps_Lock> => <Escape>
if ! pgrep fbxkb ; then
	fbxkb &
fi

