#!/usr/bin/env bash

USAGE="Usage: $(basename $0) FILE_TO_GET_RUN_CMD_FOR"

if [ $# -ne 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi

source ~/os_settings/other_files/vim_ide_common.sh

IN="$1"
OUT="${OUT_DIR}/${IN}.out"
# FLAGS=" -Wall -Wextra -Werror "
FLAGS=" -Wall -Wextra "

case "$IN" in
	*.html)
		echo -n "x-www-browser $IN"
	;;
	*.sh | *.bash | *.py | *.pl )
		echo -n "./$IN"
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo -n "$OUT"
	;;
	*.c)
		echo -n "$OUT"
	;;
	*.rs)
		echo -n "$OUT"
		# echo -n "cargo run"
	;;
	*.S)
		echo -n "cat /tmp/assemble_sh_text"
	;;
	*)
		echo -n 'echo "Default run command for this file type is not defined" && false'
		exit 1
esac

