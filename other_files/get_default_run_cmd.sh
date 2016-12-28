#!/usr/bin/env bash

USAGE="Usage: $(basename $0) IDE_DIR FILE_TO_GET_RUN_CMD_FOR"
source ~/os_settings/other_files/get_default_cmd_common.sh

case "$IN" in
	*.mk | Makefile)
		echo -n "make -f \"$IN\""
	;;
	*.html)
		echo -n "x-www-browser \"$IN\""
	;;
	*.sh | *.bash | *.py | *.pl )
		echo -n "\"./$IN\""
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo -n "\"$OUT\""
	;;
	*.c)
		echo -n "\"$OUT\""
	;;
	*.rs)
		echo -n "\"$OUT\""
		# echo -n "cargo run"
	;;
	*.nim)
		echo -n "\"$OUT\""
	;;
	*.S)
		echo -n "cat /tmp/assemble_sh_text"
	;;
	*)
		echo -n 'echo "Default run command for this file type is not defined" && false'
		exit 2
esac

