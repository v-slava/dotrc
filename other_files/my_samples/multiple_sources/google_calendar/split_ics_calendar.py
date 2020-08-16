#!/usr/bin/python3

# Settings:
in_file = 'rembuttehdon@gmail.com.ics'
out_dir = 'split_' + in_file
num_lines_per_file = 14000

import os
from enum import Enum, auto
import pathlib

os.chdir(pathlib.Path(__file__).parent.absolute())

if not os.path.exists(out_dir):
    os.makedirs(out_dir)

class State(Enum):
    HEADER = auto()
    APPENDING = auto()
    END_EVENT = auto()
    FOOTER = auto()

begin = 'BEGIN:VEVENT\n'
end = 'END:VEVENT\n'
header = []
footer = []
line_num = 0
file_num = 0
state = State.HEADER
next_file = False
f = None

def get_file_path(num):
    return os.path.join(out_dir, f'{num:03d}.ics')

def new_file():
    global file_num, f, header
    file_num = file_num + 1
    f = open(get_file_path(file_num), 'w', encoding = "utf-8")
    for l in header:
        f.write(l)

for line in open(in_file, encoding = "utf-8"):
    line_num = line_num + 1

    if state == State.HEADER:
        if line == begin:
            state = State.APPENDING
            new_file()
            f.write(line)
        else:
            header += [line]

    elif state == State.APPENDING:
        f.write(line)
        if line_num % num_lines_per_file == 0:
            next_file = True
        if line == end:
            state = State.END_EVENT

    elif state == State.END_EVENT:
        if line == begin:
            state = state.APPENDING
            if next_file:
                next_file = False
                f.close()
                new_file()
            f.write(line)
            if line_num % num_lines_per_file == 0:
                next_file = True
        else:
            state = State.FOOTER
            f.close()
            footer += [line]

    elif state == State.FOOTER:
        footer += [line]

    else:
        raise Exception('Got unexpected state')

for i in range(1, file_num + 1):
    with open(get_file_path(i), 'a', encoding = "utf-8") as f:
        for line in footer:
            f.write(line)


