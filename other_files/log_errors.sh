#!/usr/bin/env bash

# Note: this script is intended for vim IDE.

# Create files with information about compilation / linking errors.
# Sample usage:
# cd some_dir && make -j9 2>&1 | ~/os_settings/other_files/log_errors.sh ; echo ${PIPESTATUS[0]} > /tmp/vim_ide_dir/build_result

# Files:
# $OUT_DIR/source_locations - source locations of error (each in separate line)
# $OUT_DIR/message_error_0 - error message for first error (and further errors)
# $OUT_DIR/message_error_1 - error message for second error (and further errors)
# ...

set -e

OUT_DIR=/tmp/vim_ide_dir
FULL_FILE=$OUT_DIR/full_file

rm -rf $OUT_DIR
mkdir $OUT_DIR

# Write all output in $FULL_FILE and get space-separated line numbers of errors
# in $ERROR_LINES:
ERROR_LINES=$(tee $FULL_FILE | grep -n 'error:' | cut -d':' -f1)

error_index=0
for error_line in $ERROR_LINES ; do
	sed "${error_line}q;d" $FULL_FILE | grep -o '^[^:]\+:[0-9]\+:[0-9]\+' >> $OUT_DIR/source_locations
	tail -n +$error_line $FULL_FILE > $OUT_DIR/message_error_${error_index}
	error_index=$((error_index + 1))
done

