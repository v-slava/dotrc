#!/bin/bash

DIR="$HOME/my/screenshots"
FILE="$DIR/$(date '+%Y_%m_%d__%H_%M_%S_%N')_scrot.png"

EDIT_IMAGE=false
PRINT_FILE_PATH=false
SLEEP=

POSITIONAL=()
while [ $# -gt 0 ] ; do
    case "$1" in
        "--my-sleep") shift; SLEEP="$1" ; shift ;;
        "--my-print-file-path") PRINT_FILE_PATH=true ; shift ;;
        "--my-edit-image") EDIT_IMAGE=true ; shift ;;
        *) POSITIONAL+=("$1") ; shift ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

set -e

notify()
{
    echo "$@" > /tmp/i3_status_fifo
}

if [ -n "$SLEEP" ]; then
    for i in $(seq $SLEEP -1 1) ; do
        notify "screenshot in $i seconds..."
        sleep 1
    done
fi

mkdir -p "$DIR"
scrot "$@" "$FILE"
if [ "$PRINT_FILE_PATH" = "true" ]; then
    echo "$FILE"
fi
if [ -n "$SLEEP" ]; then
    if [ "$EDIT_IMAGE" != "true" ]; then
        notify "screenshot done!"
        sleep 1
    fi
    notify ""
fi
if [ "$EDIT_IMAGE" = "true" ]; then
    kolourpaint "$FILE"
fi
