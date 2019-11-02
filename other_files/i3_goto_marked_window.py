#!/usr/bin/python3

import json
import subprocess

ret = subprocess.run(['i3-msg', '-t', 'get_marks'], check = True,
        capture_output = True, encoding = 'ascii')
marks = '\n'.join(json.loads(ret.stdout))
cmd = ['dmenu', '-fn', 'Inconsolata LGC-16:monospace', '-p', 'Go to window:']
ret = subprocess.run(cmd, check = True, input = marks, capture_output = True,
        encoding = 'ascii')
selected_mark = ret.stdout[:-1]
cmd = ['i3-msg', f'[con_mark="{selected_mark}"]', 'focus']
subprocess.run(cmd, check = True, stdout = subprocess.DEVNULL)
