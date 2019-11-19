#!/usr/bin/python3

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description =
            'Preprocess stdin to stdout.')
    parser.add_argument('regex', metavar = 'PATTERN',
            help = 'regular expression to be used for file inclusion')
    parser.add_argument('include_dirs', metavar = 'INCLUDE_DIR', nargs = '+',
            help = 'directory to add to include search path')
    return parser.parse_args()

def preprocess(in_fp, out_fp, regex, include_dirs):
    import re
    import os
    include_re = re.compile(regex)
    line = in_fp.readline()
    while line:
        match = include_re.match(line)
        if match:
            included_file = match.group(1)
            file_path = None
            for d in include_dirs:
                if included_file in os.listdir(d):
                    file_path = os.path.join(d, included_file)
                    break
            if not file_path:
                raise Exception(f'file {included_file} not found in \
{include_dirs}')
            with open(file_path) as f:
                out_fp.write(f.read())
        else:
            out_fp.write(line)
        line = in_fp.readline()

def main():
    import sys
    args = parse_cmd_line_args()
    preprocess(sys.stdin, sys.stdout, args.regex, args.include_dirs)

if __name__ == '__main__':
    main()
