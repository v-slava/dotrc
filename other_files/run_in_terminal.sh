#!/bin/bash

# (my--set-shell-command-for-project 0 (concat (buffer-file-name) " top"))
# (my--set-shell-command-for-project 0 (concat (buffer-file-name) " ls -l"))

CMD="$@"

i3-msg 'focus right' > /dev/null
x-terminal-emulator -e /media/files/workspace/dotrc/other_files/run_pexpect.py $CMD
i3-msg 'focus left' > /dev/null
