#!/usr/bin/python3

import sys
import os.path
from importlib import import_module
from subprocess import check_call

def error(text, exit_code):
	sys.stderr.write(text + "\n")
	sys.exit(exit_code)

def process_extracted(arg, replace):
	prefix = "extracted_"
	if arg[:len(prefix)] != prefix:
		return arg, arg, replace
	return arg, arg[len(prefix):arg.find('.')], replace

def process_rotated_image(arg, replace):
	return "rotated_" + arg, arg, True

def process_action_external(action, arg, replace):
	try:
		return getattr(import_module("vifm_rename_" + action), "main")(arg, replace)
	except ImportError:
		return arg, arg, replace

def process_action(action, arg, replace):
	return {
		"spaces_to_underscores": (arg, arg.replace(' ', '_'), replace),
		"dots_to_underscores": (arg, os.path.splitext(arg)[0].replace('.', '_') +
		                             os.path.splitext(arg)[1], replace),
		"to_lowercase": (arg, arg.lower(), replace),
		"extracted": process_extracted(arg, replace),
		"rotated_image": process_rotated_image(arg, replace),
	}.get(action, process_action_external(action, arg, replace))

def vifm_escape(arg):
	return arg.replace(" ", "\ ")

def vifm_rename(IN, ACTION, REPLACE):
	IN, OUT, REPLACE = process_action(ACTION, IN, REPLACE)
	if not os.path.exists(IN):
		error("Input file not found: '" + IN + "'", 2)
	if OUT == IN:
		print("No need to rename input file: '" + IN + "'")
		sys.exit(0)
	out_exists = os.path.exists(OUT)
	if (not REPLACE) and out_exists:
		error("Output file already exists: '" + OUT + "'", 3)
	print("Renaming: '" + IN + "' -> '" + OUT + "'")
	# os.rename(IN, OUT)
	# vifm rename is better because vifm undo works.
	vifm_cmd = "unselect * | select "
	if out_exists:
		vifm_cmd += vifm_escape(OUT) + " | delete | select "
	vifm_cmd += vifm_escape(IN) + " | rename " + OUT
	shell_cmd = ["vifm", "--server-name", os.environ["VIFM_SERVER_NAME"],
				 "--remote", "-c", vifm_cmd]
	check_call(shell_cmd)

if __name__ == '__main__':
	usage = "Usage: " + sys.argv[0] + " FILE ACTION [-R]"
	if (len(sys.argv) < 3) or (len(sys.argv) > 4):
		error(usage, 1)
	IN = sys.argv[1]
	ACTION = sys.argv[2]
	REPLACE = False
	if (len(sys.argv) == 4):
		if (sys.argv[3] != "-R"):
			error(usage, 1)
		REPLACE = True
	vifm_rename(IN, ACTION, REPLACE)
