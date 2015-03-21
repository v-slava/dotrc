#!/usr/bin/env bash

USAGE="Usage: $(basename $0) FILE_TO_RUN"

if [ $# -ne 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi

IN="$1"
OUT="/tmp/vim_ide_${IN}.out"
LOG="/tmp/vim_ide_${IN}_log"
FLAGS=" -Wall -Wextra -Werror "

rm -f $OUT $LOG

case "$IN" in
	*.sh | *.bash)
		echo -e "Output:\n" > "$LOG"
		./$IN 1 >> "$LOG" 2>&1
		echo -e "\nExit code: $?" >> $LOG
	;;
	*.cc | *.cp | *.cxx | *.cpp | *.CPP | *.c++ | *.C)
		echo -e "Compile errors:\n" > "$LOG"
		if g++ "$IN" $FLAGS -o "$OUT" 2>>"$LOG" ; then
			echo -e "Output:\n" > "$LOG"
			"$OUT" 1 >> "$LOG" 2>&1
			echo -e "\nExit code: $?" >> $LOG
		fi
	;;
	*.c)
		echo -e "Compile errors:\n" > "$LOG"
		if gcc "$IN" $FLAGS -o "$OUT" 2>>"$LOG" ; then
			echo -e "Output:\n" > "$LOG"
			"$OUT" 1 >> "$LOG" 2>&1
			echo -e "\nExit code: $?" >> $LOG
		fi
	;;
esac

