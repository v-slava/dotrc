#!/usr/bin/python3

import os, sys, json, argparse

def parse_cmd_line_args():
    parser = argparse.ArgumentParser(description = 'Process compile_commands.json')
    parser.add_argument('-s', '--statistics', action='store_true',
                        help = 'show number of commands per compiler')
    parser.add_argument('-e', '--exclude-compilers', nargs = '+',
    help = 'compilers to exclude from compile_commands.json (changes the file)',
    metavar = 'COMPILER')
    args = parser.parse_args()
    if not any(vars(args).values()):
        parser.print_help()
        parser.exit(status = 0, message =
                '\nNo arguments provided => nothing to do. Exiting...\n')
    return args

def show_statistics():
    f_name = 'compile_commands.json'
    with open(f_name, 'r') as f:
        cmd = json.loads(f.read())
    compilers = {}
    for c in cmd:
        compiler = c['arguments'][0]
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

def filter_compilers(exclude_compilers):
    f_name = 'compile_commands.json'
    with open(f_name, 'r') as f:
        cmd_in = json.loads(f.read())
    # num = len(cmd_in)
    # print(f'before filtering: {num} commands')
    cmd_out = [c for c in cmd_in if not c['arguments'][0] in exclude_compilers]
    with open(f_name, 'w') as f:
        f.write(json.dumps(cmd_out, indent = 2))
    # num = len(cmd_out)
    # print(f'after filtering: {num} commands')

def main():
    # exclude_compilers = [
    #       'gcc',
    #       os.path.expandvars('$DOTRC_S/bin/gcc'),
    # ]
    args = parse_cmd_line_args()
    if args.statistics:
        show_statistics()
    elif args.exclude_compilers:
        print('Statistics before filtering:\n')
        show_statistics()
        filter_compilers(args.exclude_compilers)
        print('\nStatistics after filtering:\n')
        show_statistics()

if __name__ == '__main__':
    main()
