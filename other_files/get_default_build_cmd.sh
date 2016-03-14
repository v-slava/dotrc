#!/usr/bin/env bash

USAGE="Usage: $(basename $0) FILE_TO_GET_BUILD_CMD_FOR"

if [ $# -ne 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi

source ~/os_settings/other_files/vim_ide_common.sh

IN="$1"
OUT="${OUT_DIR}/${IN}.out"
# FLAGS=" -Wall -Wextra -Werror "
FLAGS=" -Wall -Wextra -std=c++11 "

case "$IN" in
	*.html)
		echo -n ""
	;;
	*.sh | *.bash | *.py | *.pl )
		echo -n "chmod +x \"$IN\""
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo -n "g++ \"$IN\" $FLAGS -o \"$OUT\""
	;;
	*.c)
		echo -n "gcc \"$IN\" $FLAGS -o \"$OUT\""
	;;
	*.rs)
		echo -n "rustc \"$IN\" -o \"$OUT\""
		# echo -n "cargo build"
	;;
	*)
		echo -n 'echo "Default build command for this file type is not defined" && false'
		exit 1
esac

