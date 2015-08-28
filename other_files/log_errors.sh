#!/usr/bin/env bash

USAGE="
Create files with information about compilation / linking errors (for vim IDE).
	Sample usage:
echo 'cd some_dir && make -j9' | bash 2>&1 | $(basename $0) nw nf ; echo -n \"Build result: \${PIPESTATUS[1]}\"
	Detailed usage:
$(basename $0) {nw|w} {nf|FILTER_EXECUTABLE}
nw                : no warnings (suppress warnings)
w                 : warnings
nf                : no filter required
FILTER_EXECUTABLE : filter executable to be used"
set -e
source ~/os_settings/other_files/vim_ide_common.sh
# Files:
# $OUT_DIR/source_locations - source locations of error (each in separate line)
# $OUT_DIR/message_error_0 - error message for first error (and further errors)
# $OUT_DIR/message_error_1 - error message for second error (and further errors)
# ...
rm -rf $OUT_DIR
mkdir $OUT_DIR

write_error()
{
	echo -e "$1" | tee -a "$FULL_FILE" 1>&2
}
usage()
{
	write_error "$USAGE"
	exit 1
}
if [ $# -ne 2 ]; then
	write_error "Exactly 2 arguments expected. Actually got $# arguments."
	usage
fi
WARNINGS="$1"
FILTER="$2"

REGEX_SRC_LOC='^[^:]\+:[0-9]\+:[0-9]\+'

if [ "$WARNINGS" = "nw" ]; then
	REGEX="${REGEX_SRC_LOC}: \(fatal error\|error\):"
else
	if [ "$WARNINGS" = "w" ]; then
		REGEX="${REGEX_SRC_LOC}: \(fatal error\|error\|warning\):"
	else
		write_error "Second argument should be either 'w' or 'nw'"
		usage
	fi
fi

# Write all output in $FULL_FILE and get space-separated line numbers of errors
# in $ERROR_LINES:
if [ "$FILTER" = "nf" ]; then
	ERROR_LINES=$(tee "$FULL_FILE" | grep -n "$REGEX" | cut -d':' -f1)
else
	if [ ! -x "$FILTER" ]; then
		write_error "Filter argument \"$FILTER\" != 'nf' and is not executable file"
		usage
	fi
	ERROR_LINES=$(tee "$FULL_FILE" | grep -n "$REGEX" | "$FILTER" | cut -d':' -f1)
fi
error_index=0
for error_line in $ERROR_LINES ; do
	sed "${error_line}q;d" "$FULL_FILE" | grep -o "$REGEX_SRC_LOC" >> "$OUT_DIR/source_locations"
	tail -n +$error_line "$FULL_FILE" > "$OUT_DIR/message_error_${error_index}"
	error_index=$((error_index + 1))
done

