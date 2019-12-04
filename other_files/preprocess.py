#!/usr/bin/python3

import re
import os

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description =
            'Preprocess stdin to stdout.')
    parser.add_argument('--allow-redefine', action = 'store_true',
            help = 'Do not issue an error on macro redefinition')
    parser.add_argument('--include-regex', metavar = 'INCLUDE_REGEX', help =
            '''Regular expression to be used for file inclusion, for example:
            ".*#INCLUDE_FILE: (\S+)". Usage example: "/* #INCLUDE_FILE
            some_file.txt */".''', nargs = '?')
    parser.add_argument('--define-regex', metavar = 'DEFINE_REGEX', help =
            '''Regular expression to be used for substitution, for example:
            ".*#DEFINE (\w+) (.+)$". Usage example: "// #DEFINE MY_ACCOUNT
            viacheslav.volkov.1".''', nargs = '?')
    parser.add_argument('--undef-regex', metavar = 'UNDEF_REGEX', help =
            '''Regular expression to be used to undo substitution, for example:
            ".*#UNDEF (\w+)". Usage example: "// #UNDEF MY_ACCOUNT".''',
            nargs = '?')
    parser.add_argument('--included-lines', metavar = 'LINE', nargs = '*',
            help = 'Predefined lines (with defines) to read before stdin.')
    parser.add_argument('--include-dirs', metavar = 'INCLUDE_DIR', nargs = '*',
            help = 'Directory to add to include search path.')
    return parser.parse_args()

def process_line(line, include_re, define_re, undef_re, allow_redefine,
        include_dirs, defines, out_fp):

    already_matched = False
    def process(regex, func):
        nonlocal already_matched
        match = regex.match(line) if regex else False
        if match:
            # if already_matched:
            #     raise Exception(f'line {line} matches multiple regexes')
            already_matched = True
            func(match)

    def define(match):
        name = match.group(1)
        definition = match.group(2)
        nonlocal defines
        if not allow_redefine and name in defines:
            raise Exception(f'macro {name} is redefined')
        defines[name] = (re.compile(name), definition)
    process(define_re, define)

    def undef(match):
        name = match.group(1)
        nonlocal defines
        del defines[name]
    process(undef_re, undef)

    if not already_matched:
        for name, (regex, substitution) in defines.items():
            line = regex.sub(substitution, line)

    def include(match):
        included_file = match.group(1)
        file_found = False
        for d in include_dirs:
            if not os.path.isdir(d):
                continue
            file_path = os.path.join(d, included_file)
            if os.path.isfile(file_path):
                file_found = True
                break
        if not file_found:
            raise Exception(f'file {included_file} not found in \
{include_dirs}')
        for line in open(file_path):
            process_line(line, include_re, define_re, undef_re, allow_redefine,
                    include_dirs, defines, out_fp)
    process(include_re, include)

    if not already_matched:
        out_fp.write(line)

def preprocess(in_fp, out_fp, include_regex, define_regex, undef_regex,
        allow_redefine, include_dirs, included_lines):
    include_re = re.compile(include_regex) if include_regex else None
    define_re = re.compile(define_regex) if define_regex else None
    undef_re = re.compile(undef_regex) if undef_regex else None
    defines = {}
    if included_lines:
        for line in included_lines:
            process_line(line + os.linesep, include_re, define_re, undef_re,
                    allow_redefine, include_dirs, defines, out_fp)
    line = in_fp.readline()
    while line:
        process_line(line, include_re, define_re, undef_re, allow_redefine,
                include_dirs, defines, out_fp)
        line = in_fp.readline()

def main():
    import sys
    args = parse_cmd_line_args()
    preprocess(sys.stdin, sys.stdout, args.include_regex, args.define_regex,
            args.undef_regex, args.allow_redefine, args.include_dirs,
            args.included_lines)

if __name__ == '__main__':
    main()
