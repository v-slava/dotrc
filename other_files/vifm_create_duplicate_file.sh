#!/bin/bash

usage()
{
	echo "Usage $(basename $0) FILE" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi

IN_FILE="$1"
if [ ! -f "$IN_FILE" ]; then
	usage
fi

TMP_FILE=/tmp/vifm_duplicate_file_tmp

set -e

echo "$IN_FILE" > "$TMP_FILE"
e --wait "$TMP_FILE"
OUT_FILE="$(cat "$TMP_FILE")"
rm "$TMP_FILE"

if [ -e "$OUT_FILE" ]; then
	echo "Error: \"$OUT_FILE\" already exists." 1>&2
	exit 2
fi
cp "$IN_FILE" "$OUT_FILE"

