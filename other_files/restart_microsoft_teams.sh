#!/bin/bash

# See:
# https://github.com/IsmaelMartinez/teams-for-linux/blob/develop/KNOWN_ISSUES.md#blank-page
# https://github.com/IsmaelMartinez/teams-for-linux/issues/171

PROCESS_NAME=teams-for-linux
if [ -z "$EXECUTABLE_NAME" ]; then
    EXECUTABLE_NAME="$PROCESS_NAME"
fi
if [ -z "$XDG_CONFIG_HOME" ]; then
    XDG_CONFIG_HOME=~/.config
fi
APP_CACHE="teams-for-linux/Partitions/teams-4-linux/Application Cache"

killall --wait -SIGKILL "$PROCESS_NAME"

if pidof "$PROCESS_NAME" 1>/dev/null ; then
    echo "Error: can't kill \"$PROCESS_NAME\"" 1>&2
    exit 1
fi

set -e

rm -rf "$XDG_CONFIG_HOME/$APP_CACHE"
nohup "$EXECUTABLE_NAME" 1>/dev/null 2>&1 &
