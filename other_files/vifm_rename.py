#!/usr/bin/python3

import sys
import os.path
from importlib import import_module
from subprocess import check_call

def error(text, exit_code):
    sys.stderr.write(text + "\n")
    sys.exit(exit_code)

def process_extracted(arg, force):
    prefix = "extracted_"
    if arg[:len(prefix)] != prefix:
        return arg, arg, force
    return arg, arg[len(prefix):arg.find('.')], force

def process_rotated_image(arg, force):
    return "rotated_" + arg, arg, True

def process_action_external(action, arg, force):
    try:
        func = getattr(import_module("vifm_rename_" + action), "main")
        return func(arg, force)
    except ImportError:
        return arg, arg, force

def process_action(action, arg, force):
    return {
        "spaces_to_underscores": (arg, arg.replace(' ', '_'), force),
        "dots_to_underscores": (arg, os.path.splitext(arg)[0].replace('.', '_')
                                     + os.path.splitext(arg)[1], force),
        "to_lowercase": (arg, arg.lower(), force),
        "extracted": process_extracted(arg, force),
        "rotated_image": process_rotated_image(arg, force),
    }.get(action, process_action_external(action, arg, force))

def vifm_select(arg):
    return "select !echo '" + arg + "'"

def vifm_rename(in_file, action, force):
    in_file, out_file, force = process_action(action, in_file, force)
    if not os.path.exists(in_file):
        error("Input file not found: '" + in_file + "'", 2)
    if out_file == in_file:
        print("No need to rename input file: '" + in_file + "'")
        sys.exit(0)
    out_exists = os.path.exists(out_file)
    if (not force) and out_exists:
        error("Output file already exists: '" + out_file + "'", 3)
    print("Renaming: '" + in_file + "' -> '" + out_file + "'")
    # os.rename(in_file, out_file)
    # vifm rename is better because vifm undo works.
    vifm_cmd = "unselect * | "
    if out_exists:
        vifm_cmd += vifm_select(out_file) + " | delete | "
    vifm_cmd += vifm_select(in_file) + " | rename " + out_file
    shell_cmd = ["vifm", "--server-name", os.environ["VIFM_SERVER_NAME"],
                 "--remote", "-c", vifm_cmd]
    check_call(shell_cmd)

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description = 'Vifm rename file(s).')
    parser.add_argument('-F', '--force', action='store_true',
                        help = 'overwrite output file(s) if any')
    parser.add_argument('action', help = 'rename action to do')
    parser.add_argument('files', nargs = '+', help = 'file(s) to be renamed',
                        metavar = 'FILE')
    return  parser.parse_args()

def main():
    args = parse_cmd_line_args()
    for f in args.files:
        vifm_rename(f, args.action, args.force)

if __name__ == '__main__':
    main()
