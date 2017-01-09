#!/usr/bin/python

import sys
import os.path
from importlib import import_module
from subprocess import check_call

usage = "Usage: " + sys.argv[0] + " FILE ACTION"

def error(text, exit_code):
	print >> sys.stderr, text
	sys.exit(exit_code)

if (len(sys.argv) != 3):
	error(usage, 1)

IN = sys.argv[1]
ACTION = sys.argv[2]

if not os.path.exists(IN):
	error("Input file not found: " + IN, 2)

def process_extracted(arg):
	prefix = "extracted_"
	if arg[:len(prefix)] != prefix:
		return arg
	return arg[len(prefix):arg.find('.')]

def process_action_external(action, arg):
	try:
		return getattr(import_module("vifm_rename_" + action), "get_output_string")(arg)
	except ImportError:
		return arg

def process_action(action, arg):
	return {
		"spaces_to_underscores": arg.replace(' ', '_'),
		"to_lowercase": arg.lower(),
		"extracted": process_extracted(arg),
	}.get(action, process_action_external(action, arg))

OUT = process_action(ACTION, IN)

if OUT == IN:
	print("No need to rename")
	sys.exit(0)

if os.path.exists(OUT):
	error("Output file already exists: " + OUT, 3)

print("Trying to rename: '" + IN + "' -> '" + OUT + "'")
# os.rename(IN, OUT)
# vifm rename is better because vifm undo works.
check_call(["vifm", "--server-name", os.environ["VIFM_SERVER_NAME"], "--remote", "-c", "rename '" + OUT + "'"])
