#!/bin/bash

LOCK_DIR=/tmp/lock_xrandr_sh
export LOG=/tmp/log_xrandr_sh_$(id -u)
export BOOTED_DIR=/tmp/xrandr_booted

if [ "$1" = "udev" ]; then
    if [ ! -d $BOOTED_DIR ]; then
        echo "Ignoring call from udev: the system is not yet booted" >> $LOG
        exit
    fi
    echo "$(realpath $0) at" | at now
    exit
fi

if mkdir $LOCK_DIR ; then
    if [ "$1" = "at" ]; then
        export DISPLAY=:0
        export DOTRC=/media/files/workspace/dotrc
        export DOTRC_S=/media/files/workspace/dotrc_s
        export HOME=$(getent passwd 1000 | cut -d: -f6)
        export XAUTHORITY=$HOME/.Xauthority
    fi
    $DOTRC/other_files/xrandr_do.sh "$@"
    RET=$?
    rm -rf $LOCK_DIR
    exit $RET
else
    echo "Ignoring xrandr.sh $@: locked." >> $LOG
fi
