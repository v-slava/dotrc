#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) {--up|--down}" 1>&2
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

case "$1" in
    "--up") OP="-inc" ;;
    "--down") OP="-dec" ;;
    "--off") sleep 0.2 && xset dpms force off ; exit 0 ;;
    *) usage
esac

CUR_VAL=$(xbacklight -get | cut -d'.' -f1)
AMOUNT=$((CUR_VAL / 10))
if [ "$AMOUNT" -eq 0 ]; then
    AMOUNT=1
fi
xbacklight $OP $AMOUNT
