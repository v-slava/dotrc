#!/bin/bash

INTERVAL=30
LOG=~/h/wait_build_log

echo "$(date): wait_build.sh started..." | tee $LOG

echo "lock_screen.sh PID:"
$DOTRC/other_files/lock_screen.sh &

if ! declare -f -F build_running ; then
    build_running()
    {
        ps -e | grep -q 'make\|Cooker\|build\.sh'
    }
fi

while build_running ; do
    sleep $INTERVAL
done
echo "$(date): build is done, updating system..." | tee -a $LOG
$DOTRC/other_files/update_system.sh
sync
echo "$(date): shutting down NOW!" | tee -a $LOG
sync
sudo poweroff
