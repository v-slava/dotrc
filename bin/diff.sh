#!/usr/bin/env bash

usage()
{
	echo -e "Usage: $(basename $0) [--no-less] LEFT RIGHT\n\
Where both LEFT and RIGHT are both either folders or files.\n" 1>&2
	exit 1
}

if [ "$1" = "--no-less" ]; then
	NO_LESS=true
	shift
fi

if [ $# -ne 2 ]; then
	usage
fi

LEFT="$1"
RIGHT="$2"
USE_COLORDIFF=true

if [ -d "$LEFT" ]; then
	if [ ! -d "$RIGHT" ]; then
		usage
	fi
	if [ -n "$USE_COLORDIFF" ]; then
		CMD="colordiff --no-dereference -ur \"$LEFT\" \"$RIGHT\" 2>&1"
		LESS_OPTIONS=-R
	else
		CMD="diff -uNr \"$LEFT\" \"$RIGHT\" 2>&1"
	fi
else
	if [ -d "$RIGHT" ]; then
		usage
	fi
	if [ -n "$USE_COLORDIFF" ]; then
		CMD="colordiff -u \"$LEFT\" \"$RIGHT\" 2>&1"
		LESS_OPTIONS=-R
	else
		CMD="diff -uN \"$LEFT\" \"$RIGHT\" 2>&1"
	fi
	# vim -d "$LEFT" "$RIGHT"
fi

if [ -n "$NO_LESS" ]; then
	eval "$CMD"
else
	eval "$CMD" | less -iN $LESS_OPTIONS
fi
