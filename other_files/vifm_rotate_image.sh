#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) IN_FILE [90|180|270]" 1>&2
    exit 1
}

if [ $# -ne 2 ]; then
    usage
fi

IN_FILE="$1"
OUT_FILE="rotated_${IN_FILE}"
DEGREES="$2"
if [ -f "$OUT_FILE" ]; then
    echo "Error: output file \"$OUT_FILE\" already exists" 1>&2
    exit 2
fi
VIFM_CMD="vifm --server-name $VIFM_SERVER_NAME --remote -c"
set -e

convert -rotate "$DEGREES" "$IN_FILE" "$OUT_FILE"
$VIFM_CMD "mark z"
$DOTRC/other_files/vifm_rename.py rotated_image "$IN_FILE"
CMD="normal 'z"
# if pidof gliv 1>/dev/null ; then
if pidof imv-wayland 1>/dev/null ; then
    # reopen image:
    CMD="${CMD}l"
fi
$VIFM_CMD "$CMD"
