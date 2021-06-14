#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This script is a simple wrapper which prefixes each i3status line with custom
# information. It is a python reimplementation of:
# http://code.stapelberg.de/git/i3status/tree/contrib/wrapper.pl
#
# To use it, ensure your $XDG_CONFIG_HOME/i3status/config contains this line:
#     output_format = "i3bar"
# in the 'general' section.
# Then, in your $XDG_CONFIG_HOME/.config_xdg/i3/config, use:
#     status_command i3status | $DOTRC/other_files/i3_status_wrapper.py
# In the 'bar' section.
#
# In its current version it will display the cpu frequency governor, but you
# are free to change it to display whatever you like, see the comment in the
# source code below.
#
# © 2012 Valentin Haenel <valentin.haenel@gmx.de>
#
# This program is free software. It comes without any warranty, to the extent
# permitted by applicable law. You can redistribute it and/or modify it under
# the terms of the Do What The Fuck You Want To Public License (WTFPL), Version
# 2, as published by Sam Hocevar. See http://sam.zoy.org/wtfpl/COPYING for more
# details.

import sys
import os
import json
import copy
import select
import tempfile
import subprocess

def get_sway_keyboard_layout():
    ret = subprocess.run(['swaymsg', '-t', 'get_inputs'], check = True,
            capture_output = True)
    inputs = json.loads(ret.stdout)
    for elem in inputs:
        if elem['identifier'] == '1:1:AT_Translated_Set_2_keyboard':
            layout_index = elem['xkb_active_layout_index']
            return ['US', 'RU', 'UA'][layout_index]
    return 'unknown keyboard layout'

def get_brightness():
    intel_backlight = '/sys/class/backlight/intel_backlight'
    if os.path.isdir(intel_backlight):
        with open(intel_backlight + '/brightness') as fp:
            cur_brightness = float(fp.readlines()[0].strip())
        with open(intel_backlight + '/max_brightness') as fp:
            max_brightness = float(fp.readlines()[0].strip())
        brightness = str(int(cur_brightness * 100 / max_brightness)) + '%'
    else:
        brightness = 'unknown'
    return 'brightness: ' + brightness

def get_final(j_stdin):
    j_full = copy.deepcopy(j_stdin)
    additional_data = [
        get_brightness(),
        # get_network_traffic(),
    ]
    for data in additional_data:
        if data:
            j_full.insert(0, {'full_text' : data})
    j_full.append({'full_text' : get_sway_keyboard_layout()})
    return j_full

def get_network_traffic():
    try:
        import subprocess
        import os
        interface = 'wlp3s0'
        if not os.path.exists('/etc/sudoers.d/vnstat'):
            return None
        dev_null = open(os.devnull, 'w')
        subprocess.check_call(['sudo', 'vnstat', '-i', interface, '-u'],
                stdout = dev_null)
        ret = subprocess.run(['vnstat', '-i', interface, '--oneline'],
                check = True, capture_output = True, encoding = 'ascii')
        rx, tx = ret.stdout.split(';')[3:5]
        return 'rx: %s | tx: %s' % (rx, tx)
    except:
        return None

def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

def read_line():
    """ Interrupted respecting reader for stdin. """
    # try reading a line, removing any extra whitespace
    try:
        line = sys.stdin.readline().strip()
        # i3status sends EOF, or an empty line
        if not line:
            sys.exit(3)
        return line
    # exit on ctrl-c
    except KeyboardInterrupt:
        sys.exit()

def update(j_stdin, stdin_prefix, fifo_line):
    if not j_stdin:
        return
    j_full = copy.deepcopy(j_stdin)
    if fifo_line:
        j_full.insert(0, {'full_text' : fifo_line})
    # echo back new encoded json
    print_line(stdin_prefix + json.dumps(j_full))

def main():
    # print(get_network_traffic())
    # import sys
    # sys.exit()

    # Skip the first line which contains the version header.
    print_line(read_line())

    # The second line contains the start of the infinite array.
    print_line(read_line())

    fifo_path = os.path.join(tempfile.gettempdir(), 'i3_status_fifo')
    if os.path.exists(fifo_path):
        os.remove(fifo_path)
    os.mkfifo(fifo_path)
    # We use os.O_RDWR instead of os.O_RDONLY because we want to keep pipe open
    # when writer closes it.
    # Otherwise we will constantly get select.POLLHUP events.
    fifo_fd = os.open(fifo_path, os.O_RDWR | os.O_NONBLOCK)
    fifo = os.fdopen(fifo_fd, 'r')
    stdin_fd = sys.stdin.fileno()
    poller = select.poll()
    poller.register(fifo_fd, select.POLLIN)
    poller.register(stdin_fd, select.POLLIN)
    stdin_is_open = True

    j_stdin = None
    stdin_prefix = None
    fifo_line = None
    while (stdin_is_open):
        events = poller.poll()
        for descriptor, event in events:
            if descriptor == stdin_fd:
                if event & select.POLLIN != 0:
                    event &= ~select.POLLIN
                    stdin_line, stdin_prefix = read_line(), ''
                    # ignore comma at start of lines:
                    if stdin_line.startswith(','):
                        stdin_line, stdin_prefix = stdin_line[1:], ','
                    j_stdin = json.loads(stdin_line)
                    update(get_final(j_stdin), stdin_prefix, fifo_line)
                if event & select.POLLHUP != 0:
                    event &= ~select.POLLHUP
                    stdin_is_open = False
                assert(event == 0)
            elif descriptor == fifo_fd:
                if event & select.POLLIN != 0:
                    event &= ~select.POLLIN
                    fifo_line = fifo.readline()[:-1]
                    update(get_final(j_stdin), stdin_prefix, fifo_line)
                assert(event == 0)

if __name__ == '__main__':
    main()
