#!/usr/bin/python3

import subprocess
import json

def get_focused_workspace():
    sway = True
    program = 'swaymsg' if sway else 'i3-msg'
    ret = subprocess.run([program, '-t', 'get_workspaces'], check = True,
            capture_output = True, encoding = 'utf8')
    for workspace in json.loads(ret.stdout):
        if workspace['focused']:
            return workspace['name']
    assert(False)

def main():
    print(get_focused_workspace())

if __name__ == '__main__':
    main()
