#!/usr/bin/python

from subprocess import check_output
from string import replace
from subprocess import Popen, PIPE
from subprocess import check_call

# Get JSON-encoded list of window marks:
marks_json = check_output(["i3-msg", "-t", "get_marks"])
marks_dmenu = replace(marks_json, '","', "\n")[2:-3]
dmenu_process = Popen(['dmenu', '-fn', 'Inconsolata LGC-16:monospace', '-p', 'Go to window:'], stdin=PIPE, stdout=PIPE)
selected_mark = dmenu_process.communicate(marks_dmenu)[0][:-1]
check_call(['i3-msg', '[con_mark="' + selected_mark + '"]', 'focus'])

