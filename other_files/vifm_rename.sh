#!/bin/bash

error()
{
	echo "$1" 1>&2
	exit $2
}

usage()
{
	error "Usage: $(basename $0) FILE ACTION" 1
}

if [ $# -ne 2 ]; then
	usage
fi
IN="$1"
ACTION="$2"
SCRIPT="$(dirname $(realpath $0))/vifm_rename_${ACTION}.sh"

if [ ! -e "$IN" ]; then
	error "Input file not found: $IN" 2
fi

if [ ! -x "$SCRIPT" ]; then
	error "Script not found: $SCRIPT" 3
fi

set -e
OUT=$(echo $IN | ${SCRIPT})
set +e

if [ -z "$OUT" ]; then
	error "Wrong script: $SCRIPT" 4
fi

if [ "$IN" = "$OUT" ]; then
	echo "No need to rename"
	exit 0
fi

mv "$IN" "$OUT"
