#!/usr/bin/env bash

TEMP_FILE=/tmp/view_first_error_TMP_file

ERROR_LINES=$(tee $TEMP_FILE | grep -n 'error:' | cut -d':' -f1)
START_LINE=$(echo $ERROR_LINES | cut -d' ' -f1)
tail -n +${START_LINE} $TEMP_FILE | grep 'error:'

