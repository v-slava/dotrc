#!/bin/bash

set -e

FILE=/tmp/vifm_selected_dir
rm -f $FILE
ORIG_WORKSPACE=$(~/os_settings/other_files/i3_get_focused_workspace.sh)
i3-msg "workspace 0" 1>/dev/null && x-terminal-emulator -title 'Choose directory:' -e vifm --choose-dir $FILE
i3-msg "workspace $ORIG_WORKSPACE" 1>/dev/null

if [ "$1" = "-n" ]; then
    cat $FILE | tr -d '\n'
else
    cat $FILE
fi
rm -f $FILE
