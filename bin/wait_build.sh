#!/bin/bash

INTERVAL=30

$DOTRC/other_files/lock_screen.sh &

if ! declare -f -F build_running ; then
    build_running()
    {
        ps -e | grep -q 'make\|Cooker'
    }
fi

while build_running ; do
    sleep $INTERVAL
done
echo "$(date): shutting down after $INTERVAL seconds..." | tee /tmp/wait_build_log
sync
sleep $INTERVAL
sync
sudo poweroff
echo "powered off!"
