#!/usr/bin/env bash

# View first compilation / linking error (if there is one) or full output if no
# errors occured. Sample usage:
# make -j9 2>&1 | view_first_error.sh && ./run_executable; exit ${PIPESTATUS[0]}

FULL_FILE=/tmp/view_first_error_TMP_file

if [ "$1" = "nw" ]; then
	REGEX='error:'
else
	REGEX='\(error\|warning\):'
fi
ERROR_LINES=$(tee $FULL_FILE | grep -n "$REGEX" | cut -d':' -f1)
START_LINE=$(echo $ERROR_LINES | cut -d' ' -f1)
if [ -n "$START_LINE" ]; then
	tail -n +${START_LINE} $FULL_FILE | grep --color=always '[^/]\+\.\(c\|h\|i\|ii\|hh\|H\|hp\|hxx\|hpp\|HPP\|h++\|tcc\|cc\|cp\|cxx\|cpp\|CPP\|c++\|C\)\?:[0-9]\+:[0-9]\+: \(fatal \)\?error:\|' | less -R
else
	cat $FULL_FILE
fi

