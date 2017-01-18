#!/bin/bash

USAGE="Usage: $(basename $0) IN_ROOT_DIR OUT_ROOT_DIR"

error()
{
	echo -e "$1" 1>&2
	exit $2
}

if [ $# -ne 2 ]; then
	error "$USAGE" 1
fi

IN="$1"
if [ ! -d "$IN" ]; then
	error "\"$IN\" is not a valid directory.\n$USAGE" 2
fi
OUT="$2"
if [ ! -d "$OUT" ]; then
	error "\"$OUT\" is not a valid directory.\n$USAGE" 3
fi

set -e

cd "$IN"
DIRS_LIST=$(find -type d)
FILES_LIST=$(find -type f -o -type l)
cd "$OUT"

for dir in $DIRS_LIST ; do
	mkdir -p "$OUT/$dir"
done

for file in $FILES_LIST ; do
	if [ -e "$OUT/$file" ]; then
		rm "$OUT/$file"
	fi
	ln -s "$IN/$file" "$OUT/$file"
done
