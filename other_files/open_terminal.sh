#!/usr/bin/env bash

set -e

if [ -z "$1" ]; then
	echo "Usage: $(basename $0) PROGRAM_TO_RUN" 1>&2
	exit 1
fi

exec x-terminal-emulator -e "$@"

# Generate unique directory name in $HOME/terminal:
FULL_TERMINAL_FOLDER_PATH=$(mktemp -d --tmpdir=$HOME/terminal XXXXXX)
TERMINAL_FOLDER=$(basename $FULL_TERMINAL_FOLDER_PATH)
export TERMINAL_FOLDER

DTACH_SOCKET=$FULL_TERMINAL_FOLDER_PATH/dtach_socket
# TERMINAL_TITLE="URxvt $TERMINAL_FOLDER"
TERMINAL_TITLE="$TERMINAL_FOLDER"

x-terminal-emulator -title "$TERMINAL_TITLE" -e dtach \
	-c $DTACH_SOCKET -E "$@"

# TERMINAL_TITLE="st $TERMINAL_FOLDER"
# LOG_TERMINAL=$FULL_TERMINAL_FOLDER_PATH/log_terminal
# ERRORS_TERMINAL=$FULL_TERMINAL_FOLDER_PATH/errors_terminal

# Dump with st's option '-o':
# st -c "URxvt" -t "$TERMINAL_TITLE" -o $LOG_TERMINAL -e dtach \
# 	-c $DTACH_SOCKET -E "$@" 2>$ERRORS_TERMINAL

# Dump with script:
# st -c "URxvt" -t "$TERMINAL_TITLE" -e script -f $LOG_TERMINAL -c dtach \
# 	-c $DTACH_SOCKET -E "$@" 2>$ERRORS_TERMINAL

# CleanUp:
rm -rf $FULL_TERMINAL_FOLDER_PATH

