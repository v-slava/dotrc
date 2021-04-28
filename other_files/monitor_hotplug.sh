#!/bin/bash

set -e
LOG=/tmp/log_monitor_hotplug_$(id -u).txt
BOOTED=/tmp/sway_booted
echo "======================================== $@" >> $LOG
date '+%Y_%m_%d__%H_%M_%S_%N' >> $LOG
export SWAYSOCK=$(ls /run/user/*/sway-ipc*.sock)
GEN_CONFIG=/home/slava/.config_xdg/sway/generated_config

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
        shift
        NUM_OUTPUTS_OLD="$1"
        if [ -z "$NUM_OUTPUTS_OLD" ]; then
            echo "Error: empty \$NUM_OUTPUTS_OLD" >> $LOG
            exit 1
        fi
        . ~/.bashrc
        if [ -z "$DOTRC" ]; then
            echo "Error: empty \$DOTRC" >> $LOG
            exit 1
        fi
        NUM_OUTPUTS=$NUM_OUTPUTS_OLD
        while [ $NUM_OUTPUTS -eq $NUM_OUTPUTS_OLD ]; do
            OUTPUTS=$(swaymsg -t get_outputs | grep '    "name": "' \
                | cut -d'"' -f4)
            NUM_OUTPUTS=$(echo -e "$OUTPUTS" | wc -l)
        done
        ;;
    startup|"")
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
        # $RIGHT_OUTPUT will be put on the right side of $MAIN_OUTPUT
        # automatically.
        FOCUSED_WORKSPACE=$($DOTRC/other_files/i3_get_focused_workspace.py)
        echo "FOCUSED_WORKSPACE = |$FOCUSED_WORKSPACE|" >> $LOG
        JSON="$(swaymsg -t get_workspaces)"
        echo "step 1" >> $LOG
        WORKSPACES=($(echo -e "$JSON" | jq '.[] .name' | cut -d'"' -f2))
        echo "WORKSPACES=${WORKSPACES[@]}" >> $LOG
        OUTPUTS=($(echo -e "$JSON" | jq '.[] .output' | cut -d'"' -f2))
        echo "OUTPUTS=${OUTPUTS[@]}" >> $LOG
        NUM_WORKSPACES=${#WORKSPACES[@]}
        echo "NUM_WORKSPACES=$NUM_WORKSPACES" >> $LOG
        CMD=""
        for i in $(seq 0 $(($NUM_WORKSPACES - 1))) ; do
            WORKSPACE=${WORKSPACES[$i]}
            OUTPUT=${OUTPUTS[$i]}
            if ! [[ "$WORKSPACE" =~ ^[0-9]+$ ]] ; then
               continue # not a number
            fi
            if [ "$WORKSPACE" = "0" ]; then
                if [ "$OUTPUT" != "$MAIN_OUTPUT" ]; then
                    CMD="${CMD}workspace $WORKSPACE, "
                    CMD="${CMD}move workspace to output $MAIN_OUTPUT, "
                fi
                continue
            fi
            if [ $(($WORKSPACE % 2)) -eq 0 ]; then
                if [ "$OUTPUT" != "$RIGHT_OUTPUT" ]; then
                    CMD="${CMD}workspace $WORKSPACE, "
                    CMD="${CMD}move workspace to output $RIGHT_OUTPUT, "
                fi
            else
                if [ "$OUTPUT" != "$MAIN_OUTPUT" ]; then
                    CMD="${CMD}workspace $WORKSPACE, "
                    CMD="${CMD}move workspace to output $MAIN_OUTPUT, "
                fi
            fi
        done
        if [ -n "$CMD" ]; then
            CMD="${CMD}workspace 1, workspace 2, workspace $FOCUSED_WORKSPACE"
            echo "swaymsg $CMD" >> $LOG
            swaymsg "$CMD" 1>>$LOG 2>&1
        fi
        echo "step 2" >> $LOG
        echo > $GEN_CONFIG
        echo "output $MAIN_OUTPUT pos 0 0" >> $GEN_CONFIG
        echo "workspace 0 output $MAIN_OUTPUT" >> $GEN_CONFIG
        for WORKSPACE in $(seq 1 2 9) ; do
            echo "workspace $WORKSPACE output $MAIN_OUTPUT" >> $GEN_CONFIG
        done
        for WORKSPACE in $(seq 2 2 8) ; do
            echo "workspace $WORKSPACE output $RIGHT_OUTPUT" >> $GEN_CONFIG
        done
        swaymsg reload
        ;;
    3)
        ;;
    *)
        ;;
esac
if [ "$1" = "startup" ]; then
    # Focus initial workspace:
    # swaymsg 'workspace 1'
    swaymsg 'workspace 3, workspace 2, workspace 1'
    touch $BOOTED
    echo "$BOOTED created" >> $LOG
fi
echo "Done." >> $LOG
