#!/bin/bash

set -e

if ~/os_settings/other_files/virtual_box.sh ; then
	sudo poweroff
else
	# ~/os_settings/other_files/lock_screen.sh &
	x-terminal-emulator -title "hibernate script" -e bash -c \
'source ~/.bashrc && ~/os_settings/other_files/update_system.sh && sleep 10 && exec sudo systemctl hibernate'
fi
