#!/bin/bash

export START_FROM_GUI=true
ZENITY=true
if [ "$1" = "-s" ]; then # "-s" stands for "silent"
    ZENITY=false
    shift
fi
EXECUTABLE="$1"
shift
OUTPUT="$("$EXECUTABLE" "$@" 2>&1)"
RET=$?
if [ $RET -ne 0 ] && [ "$ZENITY" = "true" ]; then
    LOG=/tmp/start_from_gui_log___$(date '+%Y_%m_%d__%H_%M_%S_%N')
    echo -e "------------------ START ---------------------------------" >> $LOG
    echo "Command \"$EXECUTABLE $@\" failed with exit code $RET. Output:" >> $LOG
    echo -e "{$OUTPUT}" >> $LOG
    echo -e "------------------- END ----------------------------------" >> $LOG
    zenity --error --title "Command failed" --text "See $LOG for details"
fi
exit $RET
