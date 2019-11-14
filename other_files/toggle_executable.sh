#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    echo "Wrong command line" 1>&2
    exit 1
fi
for FILE in "$@" ; do
    if [ ! -f "$FILE" ]; then
        echo "Error: $FILE is not a regular file" 1>&2
        exit 1
    fi
    X_LETTER=`ls -l "$FILE" | cut -c 4`
    if [ $X_LETTER = 'x' ]; then
        chmod -x "$FILE"
    else
        chmod +x "$FILE"
    fi
done

