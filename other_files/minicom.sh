#!/usr/bin/env bash

MINICOM_FOLDER=~/temporary/minicom_log
FILE=$MINICOM_FOLDER/file_all
FIFO_MAIN=$MINICOM_FOLDER/fifo_main
FIFO_DISPLAY_ALL=$MINICOM_FOLDER/fifo_display_all
FIFO_DISPLAY_GREP=$MINICOM_FOLDER/fifo_display_grep

I3_WORKSPACE_MINICOM=4
I3_WORKSPACE_LOG=5

# GREP_OPTIONS="-i \"some info\""
GREP_OPTIONS="__AACS__"

# VIEW_CMD="stdbuf -o 0 nl"
VIEW_CMD="cat -n"

set -e

rm -rf $MINICOM_FOLDER
mkdir $MINICOM_FOLDER
mkfifo $FIFO_MAIN $FIFO_DISPLAY_ALL $FIFO_DISPLAY_GREP

# duplicate minicom output:
tee $FILE $FIFO_DISPLAY_ALL > $FIFO_DISPLAY_GREP < $FIFO_MAIN &

# Display minicom logs:
i3-msg "workspace $I3_WORKSPACE_LOG; exec exec x-terminal-emulator \
	-title \"minicom display all\" -e $VIEW_CMD $FIFO_DISPLAY_ALL"
i3-msg "workspace $I3_WORKSPACE_LOG; exec exec x-terminal-emulator \
	-title \"minicom display grep\" -e bash -c \"$VIEW_CMD $FIFO_DISPLAY_GREP \
	| grep --color=always $GREP_OPTIONS\""

# Open minicom:
i3-msg "workspace $I3_WORKSPACE_MINICOM; exec exec x-terminal-emulator \
	-title \"minicom\" -e minicom -C $FIFO_MAIN"

