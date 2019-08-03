#!/bin/bash

source ~/.bashrc
export START_FROM_GUI=true
EXECUTABLE="$1"
shift
OUTPUT="$("$EXECUTABLE" "$@" 2>&1)"
RET=$?
if [ $RET -ne 0 ]; then
    zenity --error --title "Command \"$EXECUTABLE $@\" failed with exit code $RET. Output:" \
           --text "$OUTPUT"
fi
exit $RET
