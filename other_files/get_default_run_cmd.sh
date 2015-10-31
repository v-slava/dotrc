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
	*.sh | *.bash | *.py )
		echo "./$IN"
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo "$OUT"
	;;
	*.c)
		echo "$OUT"
	;;
	*.rs)
		echo "$OUT"
	;;
	*)
		echo 'echo "Default run command for this file type is not defined" && false'
		exit 1
esac

