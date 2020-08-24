#!/bin/bash

FILE=/tmp/xrandr_log
echo 'resolution.sh has been started...' > $FILE
date >> $FILE
xrandr >> $FILE
sleep 60
date >> $FILE
xrandr --output DVI-0 --mode 1366x768
RET=$?
xrandr >> $FILE
echo "Exit code: $RET" >> $FILE
