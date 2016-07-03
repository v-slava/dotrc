#!/bin/bash

sudo -u volkov DISPLAY=:0 zenity --info --title "$(basename $0)" --text "$0 has been called due to /etc/udev/rules.d/95-external-monitor.rules"

