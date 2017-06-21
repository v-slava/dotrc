#!/usr/bin/env bash

# To view scrollbach buffer press <Ctrl>+PrintScreen.
# See also ~/.Xresources

set -e

# if [ -z "$TERMINAL_FOLDER" ]; then
# 	echo "Error: there were no \$TERMINAL_FOLDER exported."
# 	exit 1
# fi
# FILE=$HOME/terminal/$TERMINAL_FOLDER/scrollback_buffer

FILE=$(mktemp --tmpdir=/tmp scrollback_buffer_XXXXXX)

cat > $FILE
x-terminal-emulator -e vim $FILE
rm $FILE

