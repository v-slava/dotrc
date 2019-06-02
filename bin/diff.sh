#!/usr/bin/env bash

usage()
{
	echo -e "Usage: $(basename $0) LEFT RIGHT\n\
Where both LEFT and RIGHT are both either folders or files.\n" 1>&2
	exit 1
}

if [ $# -ne 2 ]; then
	usage
fi

LEFT="$1"
RIGHT="$2"

if [ -d "$LEFT" ]; then
	if [ ! -d "$RIGHT" ]; then
		usage
	fi
	# diff -uNr "$LEFT" "$RIGHT" 2>&1 | less -iN
	colordiff -ur "$LEFT" "$RIGHT" 2>&1 | less -iNR
else
	if [ -d "$RIGHT" ]; then
		usage
	fi
	# vim -d "$LEFT" "$RIGHT"
	# diff -uN "$LEFT" "$RIGHT" 2>&1 | less -i
	colordiff -u "$LEFT" "$RIGHT" 2>&1 | less -iNR
fi

