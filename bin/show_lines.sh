#!/usr/bin/env bash

USAGE="Usage: $(basename $0) START_LINE-END_LINE"

if [ $# -ne 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi

START_LINE=$(echo $1 | cut -d'-' -f1)
END_LINE=$(echo $1 | cut -d'-' -f2)
NUM_LINES=$(echo "$END_LINE - $START_LINE + 1" | bc)

tail -n +${START_LINE} | head -n $NUM_LINES

