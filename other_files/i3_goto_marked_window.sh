#!/bin/bash

# TODO get list of all available marks and feed them to dmenu.
MARK=$(dmenu -fn 'Inconsolata LGC-16:monospace' -p 'Go to window:' < /dev/null)

i3-msg "[con_mark=\"$MARK\"] focus"

