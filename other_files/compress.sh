#!/usr/bin/env bash

# Extract archive to the current directory

usage()
{
	echo "Usage: $(basename $0) FILE ..." 1>&2
	exit 1
}

if [ $# -lt 1 ]; then
	usage
fi

FILE="$1"
if [ ! -f "$FILE" ] && [ ! -d "$FILE" ]; then
	echo "Error: file/folder not found: \"$FILE\"." 1>&2
	usage
fi
echo "Compressing \"$FILE\"..."
tar cfz "${FILE}.tar.gz" "$FILE"

