#!/usr/bin/python3

# Expected argument: copy_file_to_clipboard
# Write unit tests.

# Determine command output start.
# Limit input: [start; end].
# Support output from: git, rtags, grep, git grep, gcc.

# '^Changes to be commited:[ ]*$'
# '^Changes not staged for commit:[ ]*$'
# '^Untracked files:[ ]*$'

import sys
lines = [line.rstrip('\n') for line in sys.stdin.readlines()]

with open('/tmp/stdin', 'w') as f:
    for line in lines:
        f.write(line + '\n')

import subprocess
subprocess.run(["zenity", "--info", "--title", str(sys.argv), "--text",
    '{' + '\n'.join(lines) + '}'])
