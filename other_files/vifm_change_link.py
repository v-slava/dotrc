#!/usr/bin/python3

import os, tempfile, subprocess

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description = 'Vifm change symlink(s).')
    parser.add_argument('symlinks', nargs = '+', help = 'symlinks(s) to change',
                        metavar = 'SYMLINK')
    return parser.parse_args()

def main():
    args = parse_cmd_line_args()
    delimiter = ' -> '
    with tempfile.NamedTemporaryFile(mode = 'w+', encoding = 'utf8',
            prefix = 'vifm_change_link_', suffix = '.txt', ) as f:
        for arg in args.symlinks:
            if os.path.islink(arg):
                target = os.readlink(arg)
                os.remove(arg)
                print(f'{arg}{delimiter}{target}', file = f)
        f.flush()
        subprocess.run(['e', f.name])
        f.seek(0)
        lines = f.read().splitlines()
        for line in lines:
            link, target = line.split(delimiter)
            os.symlink(target, link)

if __name__ == '__main__':
    main()
