#!/usr/bin/env python3

import subprocess
import os

cmd = ['pactl', 'list', 'sinks']
env = os.environ.copy()
env['LANG'] = 'C'
sinks = subprocess.check_output(cmd, env = env).decode('ascii')

name = 'Name: '
state_index = sinks.find('State: RUNNING')
name_index = sinks.find(name, state_index)
name_start = name_index + len(name)
newline_index = sinks.find('\n', name_start)
name = sinks[name_start : newline_index]

print(name)
