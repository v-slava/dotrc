#!/usr/bin/python3

import xmlrpc.client

import os
import sys

def import_module(file_path):
    full_path = os.path.realpath(file_path)
    directory = os.path.dirname(full_path)
    file_name = os.path.basename(full_path)
    module_name = os.path.splitext(file_name)[0]
    if directory not in sys.path:
        sys.path.insert(0, directory)
    import importlib # for python 3.1+
    return importlib.import_module(module_name)

common = import_module('interp_python_common.py')

class Rpc(xmlrpc.client.ServerProxy):
    def __init__(self):
        self.xml_rpc = xmlrpc.client.ServerProxy('http://localhost:8000')
    @staticmethod
    def dict_to_ctx(d):
        return common.ExecutionContext(d['succeeded'], d['stdout'], d['stderr'])
    def execute(self, code):
        return self.dict_to_ctx(self.xml_rpc.execute(code))

rpc = Rpc()

ctx = rpc.execute('print("hello")')
print(ctx)

ctx = rpc.execute('global i;i = 2;print(i)')
print(ctx)

ctx = rpc.execute('print(i)')
print(ctx)

ctx = rpc.execute("""
global my_zxcv
def my_zxcv(x, y):
    return x + y
""")
print(ctx)

ctx = rpc.execute('print(my_zxcv(2, 3))')
print(ctx)

ctx = rpc.execute('print(some_undefined_var)')
print(ctx)
