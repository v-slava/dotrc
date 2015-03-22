#!/usr/bin/env bash

if [ $# -ne 1 ]; then
	echo "Usage: $(basename $0) C_CONSTANT" 1>&2
	exit 1
fi

CONST=$(printf "%d\n" $1)
awk -v constant=$CONST 'BEGIN { printf "%c\n", constant; exit }'

