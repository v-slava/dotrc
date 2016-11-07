#!/bin/bash

usage()
{
	echo -e "$(basename $0) [-n] [-r] [NUM_COMMITS_BACK_FROM_HEAD]
Options mapping:
-n => --name-only
-r => --relative" 1>&2
	exit 1
}

CMD="git show"
NUM_OPTIONS_FOUND=0

while getopts "nr" arg ; do
	case "$arg" in
		n)
			CMD="$CMD --name-only"
			NUM_OPTIONS_FOUND=$((NUM_OPTIONS_FOUND + 1))
			;;
		r)
			CMD="$CMD --relative"
			NUM_OPTIONS_FOUND=$((NUM_OPTIONS_FOUND + 1))
			;;
	esac
done

while [ $NUM_OPTIONS_FOUND -gt 0 ] ; do
	NUM_OPTIONS_FOUND=$((NUM_OPTIONS_FOUND - 1))
	shift
done

if [ $# -gt 1 ]; then
	usage
fi

CMD="$CMD HEAD"
if [ -n "$1" ]; then
	CMD="${CMD}~${1}"
fi
$CMD
