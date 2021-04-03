#!/usr/bin/python3

import json
import subprocess

sway = True
if sway:
    msg = 'swaymsg'
    font_args = []
else:
    msg = 'i3-msg'
    font_args = ['-fn', 'Inconsolata LGC-16:monospace']

ret = subprocess.run([msg, '-t', 'get_marks'], check = True,
                     capture_output = True, encoding = 'ascii')
marks = '\n'.join(json.loads(ret.stdout))
cmd = ['dmenu'] + font_args + ['-p', 'Go to window:']
ret = subprocess.run(cmd, check = True, input = marks, capture_output = True,
                     encoding = 'ascii')
selected_mark = ret.stdout[:-1]
cmd = [msg, f'[con_mark="{selected_mark}"]', 'focus']
subprocess.run(cmd, check = True, stdout = subprocess.DEVNULL)
