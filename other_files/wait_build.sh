#!/bin/bash

INTERVAL=30

$DOTRC/other_files/lock_screen.sh &

while pgrep "$1" 1>/dev/null ; do
    sleep 30
done
echo "$(date): shutting down after $INTERVAL seconds..." | tee /tmp/wait_build_log
sync
sleep $INTERVAL
sync
sudo poweroff
