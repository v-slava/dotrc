#!/bin/sh

xrandr --output LVDS1 --mode 1280x800
xrandr --output HDMI1 --mode 1920x1080
xrandr --output HDMI1 --right-of LVDS1

~/os_settings/other_files/lock_screen.sh

# invoke global X session script
. /etc/X11/Xsession

