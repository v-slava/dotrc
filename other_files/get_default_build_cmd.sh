#!/usr/bin/env bash

USAGE="Usage: $(basename $0) FILE_TO_GET_BUILD_CMD_FOR"

if [ $# -ne 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi

source ~/os_settings/other_files/ide_common.sh

IN="$1"
OUT="${IDE_DIR}/${IN}.out"
# FLAGS=" -Wall -Wextra -Werror "
FLAGS=" -Wall -Wextra "

case "$IN" in
	*.mk | Makefile)
		echo -n "true"
	;;
	*.html)
		echo -n ""
	;;
	*.sh | *.bash | *.py | *.pl )
		echo -n "chmod +x \"$IN\""
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo -n "g++ \"$IN\" $FLAGS -std=c++11 -o \"$OUT\""
	;;
	*.c)
		echo -n "gcc \"$IN\" $FLAGS -o \"$OUT\""
	;;
	*.rs)
		echo -n "rustc \"$IN\" -o \"$OUT\""
		# echo -n "cargo build"
	;;
	*.nim)
		echo -n "/usr/local/bin/nim/bin/nim -d:release --opt:size c -o:\"$OUT\" --nimcache:\"${VIM_IDE_DIR}/nimcache\" \"$IN\""
		# --debuginfo --opt:speed
	;;
	*.S)
		echo -n "assemble.sh -t \"$IN\" > /tmp/assemble_sh_text"
	;;
	*)
		echo -n 'echo "Default build command for this file type is not defined" && false'
		exit 1
esac

