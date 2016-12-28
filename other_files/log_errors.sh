#!/usr/bin/env bash

USAGE="
Create files with information about compilation / linking errors (for vim IDE).
	Sample usage:
echo 'cd some_dir && make -j9' | bash 2>&1 | $(basename $0) nw nf ; echo -n \"Build result: \${PIPESTATUS[1]}\"
	Detailed usage:
$(basename $0) IDE_DIR {nw|w} {nf|FILTER_EXECUTABLE}
nw                : no warnings (suppress warnings)
w                 : warnings
nf                : no filter required
FILTER_EXECUTABLE : filter executable to be used"
source ~/os_settings/other_files/ide_common.sh
# Files:
# $IDE_DIR/build_log_line_numbers - line numbers (in build log) of warning/error
mkdir -p "$IDE_DIR"
LINE_NUMBERS_FILE="$IDE_DIR/build_log_line_numbers"
rm -f "$IDE_DIR"/run_*
rm -f "$IDE_DIR"/build_*

write_error()
{
	echo -e "$1" | tee -a "$BUILD_LOG" 1>&2
}
usage()
{
	write_error "$USAGE"
	exit 1
}
if [ $# -ne 2 ]; then
	write_error "Wrong number of arguments"
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

# Write all output in $BUILD_LOG and get space-separated line numbers of errors
# in $ERROR_LINES:
if [ "$FILTER" = "nf" ]; then
	tee "$BUILD_LOG" | grep -n "$REGEX" | cut -d':' -f1 > "$LINE_NUMBERS_FILE"
else
	if [ ! -x "$FILTER" ]; then
		write_error "Filter argument \"$FILTER\" != 'nf' and is not executable file"
		usage
	fi
	tee "$BUILD_LOG" | grep -n "$REGEX" | "$FILTER" | cut -d':' -f1 > "$LINE_NUMBERS_FILE"
fi
