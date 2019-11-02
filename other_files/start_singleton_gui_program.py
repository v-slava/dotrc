#!/usr/bin/python3

import os
import sys
import time
import json
import signal
import psutil
import argparse
import subprocess
from i3_get_focused_workspace import get_focused_workspace

def json_find_recursive(obj, func, arr):
    if func(obj):
        arr += [obj]
    if isinstance(obj, dict):
        for key in obj:
            json_find_recursive(obj[key], func, arr)
    elif isinstance(obj, list):
        for item in obj:
            json_find_recursive(item, func, arr)

def json_find_array(obj, func):
    arr = []
    json_find_recursive(obj, func, arr)
    return arr

# def json_find_single(obj, func):
#     arr = json_find_array(obj, func)
#     assert len(arr) == 1
#     return arr[0]

# def json_find_optional(obj, func):
#     arr = json_find_array(obj, func)
#     assert (len(arr) == 0) or (len(arr) == 1)
#     return None if len(arr) == 0 else arr[0]

# def is_workspace(name, obj):
#     if not isinstance(obj, dict):
#         return False
#     if not 'type' in obj:
#         return False
#     if obj['type'] != 'workspace':
#         return False
#     if not 'name' in obj:
#         return False
#     if obj['name'] != name:
#         return False
#     return True

def is_win_with_props(prop_match_func, obj):
    if not isinstance(obj, dict):
        return False
    if not 'window_properties' in obj:
        return False
    win_props = obj['window_properties']
    assert isinstance(win_props, dict)
    return prop_match_func(win_props)

def simple_prop_match(prop_name, prop_value, win_props):
    if not prop_name in win_props:
        return False
    if win_props[prop_name] == prop_value:
        return True
    return False

def find_win_with_prop(prop_name, prop_value, obj):
    def __prop_match(win_props):
        return simple_prop_match(prop_name, prop_value, win_props)
    def __is_win(obj):
        return is_win_with_props(__prop_match, obj)
    return json_find_array(obj, __is_win)

# def find_workspace(name, json_root):
#     def is_our_workspace(obj):
#         return is_workspace(name, obj)
#     return json_find_single(json_root, is_our_workspace)

def start_program(cmd, find_window, process_name = None, sleep = 0):
    if not process_name:
        process_name = cmd[0]
    i3_msg_process = None
    def my_sigterm_handler(signum, frame):
        assert(signum == signal.SIGTERM)
        if i3_msg_process:
            i3_msg_process.terminate()
        sys.exit()
    signal.signal(signal.SIGTERM, my_sigterm_handler)
    i3_msg_cmd = ['i3-msg', '-t', 'subscribe', '-m', '[ "window" ]']
    # All possible subscribe / monitor options:
    # workspace output mode window barconfig_update binding
    i3_msg_process = subprocess.Popen(i3_msg_cmd, stdout = subprocess.PIPE)
    is_running = process_name in (p.name() for p in psutil.process_iter())
    if is_running:
        sleep = 0
        subprocess.run(cmd, check = True, stdout = subprocess.DEVNULL,
                stderr = subprocess.DEVNULL)
    else:
        subprocess.Popen(cmd, stdout = subprocess.DEVNULL,
                stderr = subprocess.DEVNULL)
    while True:
        line = i3_msg_process.stdout.readline().decode('utf8')
        event = json.loads(line)
        win = find_window(event)
        if win:
            win_id = win[0]['window']
            break
    i3_msg_process.terminate()
    i3_msg_process = None
    if sleep:
        time.sleep(sleep) # skip white blinking
    focused_workspace = get_focused_workspace()
    m = f'[id={win_id}] move container to workspace {focused_workspace}, focus'
    subprocess.run(['i3-msg', m], check = True, stdout = subprocess.DEVNULL)

def start_browser():
    dotrc = os.environ['DOTRC']
    start_page_path = os.path.join(dotrc, 'other_files', 'start_page.html')
    cmd = ['google-chrome', start_page_path]
    def find_browser(obj):
        return find_win_with_prop('window_role', 'browser', obj)
    start_program(cmd, find_browser, process_name = 'chrome', sleep = 0.5)

def start_telegram():
    def find_telegram(obj):
        return find_win_with_prop('class', 'TelegramDesktop', obj)
    start_program(['telegram'], find_telegram)

def start_skype():
    def find_skype(obj):
        return find_win_with_prop('class', 'Skype', obj)
    start_program(['skypeforlinux'], find_skype)

def parse_cmd_line_args():
    desc = 'Start singleton GUI program.'
    parser = argparse.ArgumentParser(description = desc)
    programs_supported = ['browser', 'telegram', 'skype']
    parser.add_argument('program', choices = programs_supported,
            help = 'program to start')
    return parser.parse_args()

def main():
    # start_skype()
    # start_browser()
    # start_telegram()
    # import sys; sys.exit()
    args = parse_cmd_line_args()
    func = 'start_' + args.program
    globals()[func]()

if __name__ == '__main__':
    main()
