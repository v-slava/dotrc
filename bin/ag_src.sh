#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) [-nh] REGEX
Use \"-nh\" to exclude headers from search (No Headers)." 1>&2
	exit 1
}

if [ $# -lt 1 ]; then
	usage
fi

FILES='(\.cc$|\.cp$|\.cxx$|\.cpp$|\.CPP$|\.c++$|\.C$|\.tcc$|\.c$|\.i$|\.ii$|\.S$|\.s$|\.sx$'
if [ "$1" = "-nh" ]; then
	shift
else
	FILES="$(echo "${FILES}|\.hh$|\.H$|\.hp$|\.hxx$|\.hpp$|\.HPP$|\.h++$|\.h$")"
fi
FILES="$(echo "${FILES})")"
REGEX="$1"

ag "$REGEX" -G "$FILES"

