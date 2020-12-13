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

def start_program(cmd, find_window, process_name = None, single_run = False):
    def get_win_id(win):
        assert(win)
        return win[0]['window']
    def move_win_to_cur_workspace(win_id):
        focused_workspace = get_focused_workspace()
        m = f'[id={win_id}] move container to workspace {focused_workspace}, focus'
        subprocess.run(['i3-msg', m], check = True, stdout = subprocess.DEVNULL)
    if not process_name:
        process_name = cmd[0]
    is_running = process_name in (p.name() for p in psutil.process_iter())
    if is_running and single_run:
        ret = subprocess.run(['i3-msg', '-t', 'get_tree'], check = True,
                capture_output = True, encoding = 'utf8')
        win = find_window(json.loads(ret.stdout))
        assert(win)
        move_win_to_cur_workspace(get_win_id(win))
        return
    dotrc = os.environ['DOTRC']
    start_from_gui = os.path.join(dotrc, 'other_files', 'start_from_gui.sh')
    cmd = [start_from_gui] + cmd
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
    if is_running:
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
            win_id = get_win_id(win)
            break
    i3_msg_process.terminate()
    i3_msg_process = None
    move_win_to_cur_workspace(win_id)

def start_browser(args):
    def find_browser(obj):
        return find_win_with_prop('window_role', 'browser', obj)
    # start_program(['firefox'] + args, find_browser)
    start_program(['google-chrome'] + args, find_browser)

def start_email(args):
    def find_email_client(obj):
        return find_win_with_prop('class', 'Evolution', obj)
    dotrc = os.environ['DOTRC']
    email_client = os.path.join(dotrc, 'other_files', 'email_client.sh')
    start_program([email_client] + args, find_email_client,
                  process_name = 'evolution', single_run = True)

def start_telegram(args):
    def find_telegram(obj):
        return find_win_with_prop('class', 'TelegramDesktop', obj)
    start_program(['telegram'] + args, find_telegram)

def start_skype(args):
    def find_skype(obj):
        return find_win_with_prop('class', 'Skype', obj)
    start_program(['skypeforlinux'] + args, find_skype)

def start_viber(args):
    def find_viber(obj):
        return find_win_with_prop('class', 'ViberPC', obj)
    start_program(['viber'] + args, find_viber)

def start_slack(args):
    def find_slack(obj):
        return find_win_with_prop('class', 'Slack', obj)
    start_program(['slack'] + args, find_slack)

def start_dictionary(args):
    def find_dictionary(obj):
        return find_win_with_prop('class', 'GoldenDict', obj)
    start_program(['goldendict'] + args, find_dictionary)

def parse_cmd_line_args():
    desc = 'Start singleton GUI program.'
    parser = argparse.ArgumentParser(description = desc)
    programs_supported = ['browser', 'email', 'telegram', 'skype', 'viber',
            'slack', 'dictionary']
    parser.add_argument('program', choices = programs_supported,
            help = 'program to start')
    return parser.parse_known_args()

def main():
    # start_skype()
    # start_browser()
    # start_telegram()
    # start_dictionary_lookup()
    # import sys; sys.exit()
    args = parse_cmd_line_args()
    func = 'start_' + args[0].program
    globals()[func](args[1])

if __name__ == '__main__':
    main()
