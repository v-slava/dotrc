#!/bin/bash

xrandr --output LVDS1 --mode 1366x768 \
       --output HDMI1 --mode 1680x1050 --right-of LVDS1

# ~/os_settings/other_files/lock_screen.sh &

# invoke global X session script
. /etc/X11/Xsession

