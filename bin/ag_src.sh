#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) REGEX" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi
REGEX="$1"

ag "$REGEX" -G '(\.cc$|\.cp$|\.cxx$|\.cpp$|\.CPP$|\.c++$|\.C$|\.hh$|\.H$|\.hp$|\.hxx$|\.hpp$|\.HPP$|\.h++$|\.tcc$|\.h$|\.c$|\.i$|\.ii$|\.S$|\.s$\.sx$)'

