#!/bin/bash

xmodmap ~/.Xmodmap
# xcape -e 'Control_L=Escape' # Remap <Caps_Lock> => <Escape>
if ! pgrep fbxkb ; then
	fbxkb &
fi

