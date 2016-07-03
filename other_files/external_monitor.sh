#!/bin/bash

id=`id -u`
safeuser="volkov"

if [ $id = "0" ]; then
	sudo -u $safeuser $0
	exit
fi

DISPLAY=:0 zenity --info --title "$(basename $0)" --text "$0 has been called due to /etc/udev/rules.d/95-external-monitor.rules"

