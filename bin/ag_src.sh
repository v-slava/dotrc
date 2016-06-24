#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) [-h] [-s] REGEX
Use \"-h\" to search in headers only, \"-s\" to search in source files only." 1>&2
	exit 1
}

if [ $# -lt 1 ]; then
	usage
fi

SOURCES='\.cc$|\.cp$|\.cxx$|\.cpp$|\.CPP$|\.c++$|\.C$|\.tcc$|\.c$|\.i$|\.ii$|\.S$|\.s$|\.sx$'
HEADERS='\.hh$|\.H$|\.hp$|\.hxx$|\.hpp$|\.HPP$|\.h++$|\.h$'

if [ "$1" = "-h" ]; then
	FILES="($HEADERS)"
	shift
else
	if [ "$1" = "-s" ]; then
		FILES="($SOURCES)"
		shift
	else
		FILES="($HEADERS\|$SOURCES)"
	fi
fi
REGEX="$1"

ag "$REGEX" -G "$FILES"

