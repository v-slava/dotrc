#!/usr/bin/python

# (my--set-shell-command-for-project 0 "~/os_settings/other_files/run_in_terminal.sh ls -l")

import pexpect
import sys

cmd = ' '.join(sys.argv[1:])
child = pexpect.spawn('bash')
child.sendline(cmd)
child.interact()
