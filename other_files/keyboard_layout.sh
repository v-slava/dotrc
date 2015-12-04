#!/bin/bash

set -e

setxkbmap us,ru
xmodmap ~/.Xmodmap
# Remap <Caps_Lock> => <Escape>
xcape -e 'Control_L=Escape'
fbxkb

