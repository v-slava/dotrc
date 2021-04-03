#!/usr/bin/env bash

# To view scrollbach buffer press <Ctrl>+PrintScreen.
# For urxvt see: ~/.Xresources
# For foot see: ~/.config_xdg/foot/foot.ini

set -e

FILE=$(mktemp --tmpdir=/tmp scrollback_buffer_XXXXXX.txt)

cat > $FILE
$DOTRC/other_files/open_terminal.sh e $FILE
rm $FILE
