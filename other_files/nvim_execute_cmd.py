#!/usr/bin/python3

import sys, glob
from pynvim import attach

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description =
            'Save all files in all nvim instances.')
    parser.add_argument('--exclude', metavar = 'SOCKET', help =
            'Exclude given nvim socket path')
    parser.add_argument('cmd', metavar = 'CMD', help = 'command to be executed')
    return parser.parse_args()

def main():
    args = parse_cmd_line_args()
    sockets = glob.glob("/tmp/nvim*/0")
    if args.exclude:
        sockets.remove(args.exclude)
    for socket in sockets:
        # nvim = attach('tcp', address = '127.0.0.1', port = 7777)
        # nvim = attach('socket', path = '/tmp/nvimsocket')
        nvim = attach('socket', path = socket)
        # nvim.command('echo "Hello world!"')
        nvim.command(args.cmd)

if __name__ == '__main__':
    main()
