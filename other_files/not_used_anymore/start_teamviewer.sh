#!/usr/bin/env bash

# bindsym $Alt_R+$Shift_R+t exec --no-startup-id exec $START $DOTRC/other_files/start_teamviewer.sh

set -e
sudo systemctl start teamviewerd.service
teamviewer
