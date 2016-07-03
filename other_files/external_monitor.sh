#!/bin/bash

id=`id -u`
safeuser="volkov"

if [ $id = "0" ]; then
	sudo -u $safeuser $0
	exit
fi

DISPLAY=:0 zenity --info --title "$(basename $0)" --text "$0 has been called due to /etc/udev/rules.d/95-external-monitor.rules"

# master:
if xrandr | grep -q 'HDMI1 connected' ; then
	xrandr --output HDMI1 --mode 1680x1050 --right-of LVDS1
else
	xrandr --output HDMI1 --off
fi

