#!/usr/bin/python

import sys
import subprocess

usage = "Usage: " + sys.argv[0] + " {left|right|up|down}"

if (len(sys.argv) != 2):
	print >> sys.stderr, usage
	sys.exit(1)

def get_direction(arg):
	valid = {
		"left": "left",
		"right": "right",
		"top": "top",
		"bottom": "bottom",
	}
	dir = valid.get(arg, "incorrect_argument")
	if (dir == "incorrect_argument"):
		print >> sys.stderr, usage
		sys.exit(1)
	return dir

direction = get_direction(sys.argv[1])

def i3_msg(msg):
	return subprocess.check_output(["i3-msg", msg])

def i3_msg_success(msg):
	result = i3_msg(msg)
	if (result != "[{\"success\":true}]\n"):
		print >> sys.stderr, "i3-msg \"" + msg + "\" failed"
		sys.exit(2)

i3_msg_success("focus " + direction)
# print i3_msg("focus " + direction)
# print "OK"

