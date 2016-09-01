#!/bin/bash

set -e

~/os_settings/other_files/lock_screen.sh &
x-terminal-emulator -title "hibernate script" -e bash -c \
'source ~/.bashrc && ~/os_settings/other_files/update_system.sh && exec sudo systemctl hibernate'

