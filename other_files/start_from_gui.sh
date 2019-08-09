#!/bin/bash

source ~/.bashrc
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
    zenity --error --title "Command \"$EXECUTABLE $@\" failed with exit code $RET. Output:" \
           --text "$OUTPUT"
fi
exit $RET
