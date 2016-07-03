#!/bin/bash

# master:
if [ "$1" = "xinitrc" ]; then
	xrandr --output LVDS1 --mode 1366x768
fi
if xrandr | grep -q 'HDMI1 connected' ; then
	xrandr --output HDMI1 --mode 1680x1050 --right-of LVDS1
else
	if [ "$1" = "external_monitor.sh" ]; then
		xrandr --output HDMI1 --off
	fi
fi

