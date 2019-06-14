#!/bin/bash

usage()
{
    echo "$(basename $0) [-h] [-m MARKER] [-os SCRIPT_FILE] [-ob BINARY_FILE] IN_FILE"
    exit 1
}

# default values:
MARKER="MARKER:"
OUT_SCRIPT="script.sh"
OUT_BINARY="data.bin"

while [ $# -ne 0 ] ; do
    case "$1" in
        "-h") usage ;;
        "-m") shift ; MARKER="$1" ;;
        "-os") shift ; OUT_SCRIPT="$1" ;;
        "-ob") shift ; OUT_BINARY="$1" ;;
        *)
            if [ -n "$IN_FILE" ]; then
                usage
            fi
            IN_FILE="$1"
            ;;
    esac
    shift
done

if [ -z "$IN_FILE" ]; then
    usage
fi

set -e

MARKER_LINE_NUM=$(grep -n -a -m1 "^${MARKER}$" "$IN_FILE" | cut -d: -f1)
if [ -z "$MARKER_LINE_NUM" ]; then
    echo "Error: MARKER not found in input file" 1>&2
    exit 1
fi

head -n $MARKER_LINE_NUM "$IN_FILE" > "$OUT_SCRIPT"
tail -n +$((MARKER_LINE_NUM + 1)) "$IN_FILE" > "$OUT_BINARY"
set -x
file "$OUT_BINARY"
