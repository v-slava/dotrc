#!/bin/bash

set -e
LOG=/tmp/log_monitor_hotplug_$(id -u).txt
BOOTED=/tmp/sway_booted
echo "======================================== $@" >> $LOG
date '+%Y_%m_%d__%H_%M_%S_%N' >> $LOG
export SWAYSOCK=$(ls /run/user/*/sway-ipc*.sock)

case "$1" in
    udev)
        if [ ! -f $BOOTED ]; then
            echo "Not yet booted => skip." >> $LOG
            exit
        fi
        NUM_OUTPUTS_OLD=$(swaymsg -t get_outputs | grep '    "name": "' | wc -l)
        echo "NUM_OUTPUTS_OLD=$NUM_OUTPUTS_OLD" >> $LOG
        echo "$(realpath $0) udev_at_now $NUM_OUTPUTS_OLD" | at now
        echo "Done." >> $LOG
        exit
        ;;
    udev_at_now)
        NUM_OUTPUTS_OLD="$2"
        if [ -z "$NUM_OUTPUTS_OLD" ]; then
            echo "Error: empty \$NUM_OUTPUTS_OLD" >> $LOG
            exit 1
        fi
        NUM_OUTPUTS=$NUM_OUTPUTS_OLD
        while [ $NUM_OUTPUTS -eq $NUM_OUTPUTS_OLD ]; do
            OUTPUTS=$(swaymsg -t get_outputs | grep '    "name": "' \
                | cut -d'"' -f4)
            NUM_OUTPUTS=$(echo -e "$OUTPUTS" | wc -l)
        done
        ;;
    startup)
        OUTPUTS=$(swaymsg -t get_outputs | grep '    "name": "' | cut -d'"' -f4)
        NUM_OUTPUTS=$(echo -e "$OUTPUTS" | wc -l)
        ;;
    *)
        echo "Error: wrong arg: $1" >> $LOG
        exit 1
esac

echo -n "OUTPUTS:" >> $LOG
for OUTPUT in $OUTPUTS ; do
    echo -n " |$OUTPUT|" >> $LOG
done
echo -e "\nNUM_OUTPUTS=|$NUM_OUTPUTS|" >> $LOG

MAIN_OUTPUT=$(echo -e "$OUTPUTS" | grep eDP)
echo "MAIN_OUTPUT=|$MAIN_OUTPUT|" >> $LOG

case $NUM_OUTPUTS in
    1)
        for WORKSPACE in $(seq 0 9) ; do
            swaymsg "workspace $WORKSPACE output $MAIN_OUTPUT"
        done
        ;;
    2)
        RIGHT_OUTPUT=$(echo -e "$OUTPUTS" | grep -v $MAIN_OUTPUT)
        echo "RIGHT_OUTPUT=|$RIGHT_OUTPUT|" >> $LOG
        swaymsg "output $MAIN_OUTPUT pos 0 0"
        # $RIGHT_OUTPUT will be put on the right side of $MAIN_OUTPUT
        # automatically.
        swaymsg "workspace 0 output $MAIN_OUTPUT"
        for WORKSPACE in $(seq 1 2 9) ; do
            swaymsg "workspace $WORKSPACE output $MAIN_OUTPUT"
        done
        for WORKSPACE in $(seq 2 2 8) ; do
            swaymsg "workspace $WORKSPACE output $RIGHT_OUTPUT"
        done
        ;;
    3)
        ;;
    *)
        ;;
esac
if [ "$1" = "startup" ]; then
    swaymsg 'workspace 3'
    swaymsg 'workspace 2'
    # Focus initial workspace:
    swaymsg 'workspace 1'
    touch $BOOTED
    echo "$BOOTED created" >> $LOG
fi
echo "Done." >> $LOG
