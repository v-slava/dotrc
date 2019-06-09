#!/bin/bash

INTERVAL=30
LOG=/tmp/wait_build_log

echo "$(date): wait_build.sh started..." | tee $LOG

echo "lock_screen.sh PID:"
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
echo "$(date): shutting down after $INTERVAL seconds..." | tee -a $LOG
sync
sleep $INTERVAL
echo "$(date): shutting down NOW!" | tee -a $LOG
sync
sudo poweroff
