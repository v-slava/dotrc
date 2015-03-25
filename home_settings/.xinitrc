#!/bin/sh

xrandr --output HDMI1 --mode 1920x1080 --output LVDS1 --mode 1280x800 --left-of HDMI1

~/os_settings/other_files/lock_screen.sh

# invoke global X session script
. /etc/X11/Xsession

