#!/bin/bash

LOCK_DIR=/tmp/lock_xrandr_sh
LOG=/tmp/log_xrandr_sh_$(id -u)

if mkdir $LOCK_DIR ; then
    $DOTRC/other_files/xrandr_do.sh "$@"
    RET=$?
    rm -rf $LOCK_DIR
    exit $RET
else
    echo "Ignoring xrandr.sh $@: locked." >> $LOG
fi
