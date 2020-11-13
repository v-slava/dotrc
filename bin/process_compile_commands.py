#!/usr/bin/python3

import os, sys, json, argparse

def parse_cmd_line_args():
    parser = argparse.ArgumentParser(description = 'Process compile_commands.json')
    def_file = 'compile_commands.json'
    parser.add_argument('-i', '--input-file', default = def_file,
            help = f'file to be processed (by default: {def_file})',
            metavar = 'FILE')
    parser.add_argument('-s', '--statistics', action='store_true',
                        help = 'show number of commands per compiler')
    parser.add_argument('-e', '--exclude-compilers', nargs = '+',
    help = 'compilers to exclude from compile_commands.json (changes the file)',
    metavar = 'COMPILER')
    args = parser.parse_args()
    if not args.statistics and not args.exclude_compilers:
        parser.print_help()
        parser.exit(status = 0, message =
                '\nNo arguments provided => nothing to do. Exiting...\n')
    return args

def get_compiler(cmd_json):
    # Old compile_commands.json format:
    # [
    #   {
    #     "command": "gcc -c main.c ..."
    #     "directory": "/some/dir",
    #     "file": "/some/dir/main.c",
    #   },
    #   ... (no comma after last item)
    # ]
    # New compile_commands.json format:
    # [
    #   {
    #     "arguments": [ "gcc", "-c", "main.c", ... ],
    #     "directory": "/some/dir",
    #     "file": "/some/dir/main.c",
    #     "output": "/some/dir/main.o"
    #   },
    #   ... (no comma after last item)
    # ]
    if 'arguments' in cmd_json:
        return cmd_json['arguments'][0]
    else:
        return cmd_json['command'].split()[0]

def show_statistics(input_file):
    with open(input_file, 'r') as f:
        cmd = json.loads(f.read())
    compilers = {}
    for c in cmd:
        compiler = get_compiler(c)
        if compiler in compilers:
            compilers[compiler] = compilers[compiler] + 1
        else:
            compilers[compiler] = 1
    for compiler in compilers:
        num = compilers[compiler]
        print(f'{compiler} : {num}')
    # compilers = set()
    # for c in cmd:
    #     compiler = c['arguments'][0]
    #     compilers.add(compiler)
    # print('\n'.join(compilers)

def filter_compilers(input_file, exclude_compilers):
    with open(input_file, 'r') as f:
        cmd_in = json.loads(f.read())
    # num = len(cmd_in)
    # print(f'before filtering: {num} commands')
    cmd_out = [c for c in cmd_in if not get_compiler(c) in exclude_compilers]
    with open(input_file, 'w') as f:
        f.write(json.dumps(cmd_out, indent = 2))
    # num = len(cmd_out)
    # print(f'after filtering: {num} commands')

def main():
    # exclude_compilers = [
    #       'gcc',
    #       os.path.expandvars('$DOTRC_S/bin/gcc'),
    # ]
    args = parse_cmd_line_args()
    if args.exclude_compilers:
        print('Statistics before filtering:\n')
        show_statistics(args.input_file)
        filter_compilers(args.input_file, args.exclude_compilers)
        print('\nStatistics after filtering:\n')
        show_statistics(args.input_file)
    elif args.statistics:
        show_statistics(args.input_file)

if __name__ == '__main__':
    main()
