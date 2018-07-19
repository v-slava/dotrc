#!/bin/bash

set -e

$DOTRC/other_files/lock_screen.sh &
x-terminal-emulator -title "suspend script" -e bash -c \
'source ~/.bashrc && $DOTRC/other_files/update_system.sh && exec sudo systemctl suspend'

