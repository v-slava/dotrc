#!/bin/bash

INTERVAL=30

$DOTRC/other_files/lock_screen.sh

while pgrep "$1" 1>/dev/null ; do
    sleep 30
done
echo "Shutting down after $INTERVAL seconds..."
sync
sleep $INTERVAL
sync
sudo poweroff
