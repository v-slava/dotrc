#!/usr/bin/python3

from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler

import io
import contextlib
import traceback

import sys
import os

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

# Restrict to a particular path.
class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = ('/RPC2',)

def execute(code):
    @contextlib.contextmanager
    def get_std_io():
        orig_streams = (sys.stdout, sys.stderr)
        new_streams = (io.StringIO(), io.StringIO())
        (sys.stdout, sys.stderr) = new_streams
        yield new_streams
        (sys.stdout, sys.stderr) = orig_streams
    succeeded = True
    with get_std_io() as s:
        try:
            exec(code)
        except:
            succeeded = False
            traceback.print_exc()
    return common.ExecutionContext(succeeded, s[0].getvalue(), s[1].getvalue())

# Create server
with SimpleXMLRPCServer(('localhost', 8000),
                        requestHandler=RequestHandler) as server:
    server.register_introspection_functions()
    server.register_function(execute)
    # Run the server's main loop
    server.serve_forever()
