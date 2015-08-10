#!/usr/bin/env bash

# View first compilation / linking error (if there is one) or full output if no
# errors occured. Sample usage:
# make -j9 2>&1 | view_first_error.sh && ./run_executable; exit ${PIPESTATUS[0]}

TEMP_FILE=/tmp/view_first_error_TMP_file

ERROR_LINES=$(tee $TEMP_FILE | grep -n 'error:' | cut -d':' -f1)
START_LINE=$(echo $ERROR_LINES | cut -d' ' -f1)
if [ -n "$START_LINE" ]; then
	tail -n +${START_LINE} $TEMP_FILE | grep --color=always '[^/]\+\.\(c\|h\|i\|ii\|hh\|H\|hp\|hxx\|hpp\|HPP\|h++\|tcc\|cc\|cp\|cxx\|cpp\|CPP\|c++\|C\)\?:[0-9]\+:[0-9]\+: \(fatal \)\?error:\|' | less -R
else
	cat $TEMP_FILE
fi

