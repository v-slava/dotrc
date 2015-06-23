#!/usr/bin/env bash

# View first compilation / linking error (if there is one) or full output if no
# errors occured. Sample usage:
# make -j9 2>&1 | view_first_error.sh && ./run_executable

TEMP_FILE=/tmp/view_first_error_TMP_file

ERROR_LINES=$(tee $TEMP_FILE | grep -n 'error:' | cut -d':' -f1)
# echo "ERROR_LINES=|$ERROR_LINES|"
START_LINE=$(echo $ERROR_LINES | cut -d' ' -f1)
# echo "START_LINE=|$START_LINE|"
if [ -n "$START_LINE" ]; then
	tail -n +${START_LINE} $TEMP_FILE | grep --color=always '[^/]\+\.c\(pp\|xx\)\?:[0-9]\+:[0-9]\+: error:\|' | less -R
	exit 1
else
	cat $TEMP_FILE
fi

