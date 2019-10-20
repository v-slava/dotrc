#!/usr/bin/python3

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | silent !" . expand("%:p") . ' > /tmp/'
# \ . expand("%:t") . " && if pgrep -x xdot > /dev/null ; then true ; else "
# \ . "xdot /tmp/" . expand("%:t") . " & fi"
# EVAL REGION ENDS HERE.

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmd " . expand("%:p")
# EVAL REGION ENDS HERE.

import graphviz

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

g = CallGraph(repo = 'https://github.com/v-slava/dotrc/blob/master',
        name = 'call graph')
g.attr(label = 'Legend: black - good, red - bad')
g.func('import_module', 'other_files/interp_python/client.py', 8, color = 'red')
g.noret_func('__main__', 'other_files/interp_python/client.py', 18)
g.call('__main__', 'import_module', label = 'some call')
g.calls([('a', 'b'), ('b', 'c'), ('a', 'c')])

# g.subgraph(g)

with g.subgraph(name='cluster_0') as sg:
    sg.attr(label='process #1')
    sg.attr(style='filled', color='grey')
    sg.node_attr.update(style = 'filled', color = 'lightgrey')
    sg.edges([('a0', 'a1'), ('a1', 'a2'), ('a2', 'a3')])

print(g.source)
