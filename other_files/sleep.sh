#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) SLEEP_SECONDS [TEXT]" 1>&2
    exit 1
}

SLEEP="$1"
if [ -z "$SLEEP" ]; then
    usage
fi
TEXT="$2"
if [ -z "$TEXT" ]; then
    TEXT="sleeping"
fi

for i in $(seq $SLEEP -1 1) ; do
    echo "$TEXT $i seconds..."
    sleep 1
done
echo "$TEXT now!"
