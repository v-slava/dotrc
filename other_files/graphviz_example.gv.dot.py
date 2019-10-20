#!/usr/bin/python3

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmdNoOpen ./" . expand("%:t")
# \ . " -f svg -o /media/sf_virtualbox_shared/" . expand("%:t") . '.svg'
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmdNoOpen ./" . expand("%:t")
# \ . " -f pdf -o /media/sf_virtualbox_shared/" . expand("%:t") . '.pdf'
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmdNoOpen ./" . expand("%:t")
# \ . " -v xdot"
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmdNoOpen ./" . expand("%:t")
# \ . " -v"
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmd ./" . expand("%:t") . " -f gv"
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmd ./" . expand("%:t") . " -h"
# EVAL REGION ENDS HERE.

import graphviz
import sys

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description = 'Call graph generator.')
    parser.add_argument('-v', '--view', nargs = '?', metavar = 'xdot',
            choices = ['xdot'], default = argparse.SUPPRESS,
            help = 'view the graph')
    parser.add_argument('-f', '--format', choices = ['gv', 'svg', 'pdf'],
            help = 'output format')
    parser.add_argument('-o', '--output-file', metavar = 'FILE',
            help = 'output file (if not specified => use stdout)')
    args =  parser.parse_args()
    # print(args)
    if args.output_file and (not args.format):
        message = 'error: output file is specified but format is missing'
        sys.stderr.write(message + '\n')
        parser.print_usage(sys.stderr)
        sys.exit(1)
    if (not args.format) and (not hasattr(args, 'view')):
        print('nothing to do')
        parser.print_usage(sys.stdout)
        sys.exit(0)
    return args

class CallGraph(graphviz.Digraph):
    def __init__(self, repo = None, **args):
        self.repo = repo + '/' if repo else ''
        super(CallGraph, self).__init__(**args)
    def __get_url(self, file, line):
        return self.repo + file + '#L' + str(line)
    def __func(self, name, label, file, line, **attrs):
        self.node(name, label, href = self.__get_url(file, line), **attrs)
    def func(self, name, file, line, **attrs):
        self.__func(name, name + '()', file, line, **attrs)
    def noret_func(self, name, file, line, **attrs):
        self.__func(name, '__noreturn ' + name + '()', file, line, **attrs)
    def call(self, caller, callee, **attrs):
        self.edge(caller, callee, **attrs)
    def calls(self, calls):
        self.edges(calls)

def construct_graph():
    g = CallGraph(repo = 'https://github.com/v-slava/dotrc/blob/master',
            filename = 'hello.gv',
            name = 'call graph')
    g.attr(label = 'Legend: black - good, red - bad')
    g.func('import_module', 'other_files/interp_python/client.py', 8,
            color = 'red')
    g.noret_func('__main__', 'other_files/interp_python/client.py', 18)
    g.call('__main__', 'import_module', label = 'some call')
    g.calls([('a', 'b'), ('b', 'c'), ('a', 'c')])
    # g.subgraph(g)
    with g.subgraph(name = 'cluster_0') as sg:
        sg.attr(label = 'process #1')
        sg.attr(style = 'filled', color = 'grey')
        sg.node_attr.update(style = 'filled', color = 'lightgrey')
        sg.edges([('a0', 'a1'), ('a1', 'a2'), ('a2', 'a3')])
    return g

def main():
    args = parse_cmd_line_args()
    g = construct_graph()
    if args.format:
        if args.format == 'gv':
            binary_output = False
            data = g.source
        else:
            binary_output = True
            data = g.pipe(format = args.format)
        if args.output_file:
            mode = 'wb' if binary_output else 'w'
            with open(args.output_file, mode) as f:
                f.write(data)
        else:
            if binary_output:
                sys.stdout.buffer.write(data)
            else:
                sys.stdout.write(data)
    if hasattr(args, 'view'):
        if args.view == 'xdot':
            import os
            import tempfile
            this_script_file_name = os.path.basename(__file__)
            file_name = this_script_file_name + '.gv'
            file_path = os.path.join(tempfile.gettempdir(), file_name)
            with open(file_path, 'w') as f:
                f.write(g.source)
            import psutil
            if "xdot" in (p.name() for p in psutil.process_iter()):
                # Assume "xdot" is already running on the file we've just
                # written => there is no need to run "xdot" again.
                sys.exit(0)
            import subprocess
            subprocess.Popen(['xdot', file_path], close_fds = True)
        else:
            g.view()

if __name__ == "__main__":
    main()