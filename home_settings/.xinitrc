#!/bin/sh

# Other keyboard mappings:
xmodmap ~/.Xmodmap
# Remap <Caps_Lock> => <Escape>
xcape -e 'Control_L=Escape'

xrandr --output LVDS1 --mode 1280x800 \
       --output HDMI1 --mode 1920x1080 --right-of LVDS1 --rotate left

# ~/os_settings/other_files/lock_screen.sh

# invoke global X session script
. /etc/X11/Xsession

