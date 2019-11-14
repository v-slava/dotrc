#!/usr/bin/python3

# Dependencies: python3-psutil python3-daemon

from xmlrpc.server import SimpleXMLRPCServer
from xmlrpc.server import SimpleXMLRPCRequestHandler

import io
import contextlib
import traceback

import daemon
import logging
import logging.handlers

import sys
import os
import shutil

def import_module(file_path):
    full_path = os.path.realpath(file_path)
    directory = os.path.dirname(full_path)
    file_name = os.path.basename(full_path)
    module_name = os.path.splitext(file_name)[0]
    if directory not in sys.path:
        sys.path.insert(0, directory)
    import importlib # for python 3.1+
    return importlib.import_module(module_name)

def set_process_name(name):
    from ctypes import cdll, byref, create_string_buffer
    libc = cdll.LoadLibrary('libc.so.6') # load a C library
    buf = create_string_buffer(len(name) + 1)
    buf.value = name.encode('ascii') # null terminated string
    # refer to "#define" of "/usr/include/linux/prctl.h" for the misterious
    # value 16 & arg[3..5] are zero as the man page says:
    libc.prctl(15, byref(buf), 0, 0, 0)

def killall(process_name):
    import psutil
    for proc in psutil.process_iter():
        if proc.name() == process_name:
            proc.kill()

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
    global request_num
    request_num = request_num + 1
    log.info('Request #' + str(request_num) + '. Executing code:\n' + separator + '\n' + code + '\n' + separator)
    succeeded = True
    with get_std_io() as s:
        try:
            exec(code)
        except:
            succeeded = False
            traceback.print_exc()
    ctx = common.ExecutionContext(succeeded, s[0].getvalue(), s[1].getvalue())
    log.info('Result: ' + str(ctx))
    return ctx

def main():
    process_name = 'interp_python'
    killall(process_name) # kill previous instance if running
    set_process_name(process_name)
    tmp_dir = '/tmp/interp_python'
    shutil.rmtree(tmp_dir, ignore_errors = True)
    os.mkdir(tmp_dir)
    # with open(os.path.join(tmp_dir, 'pid'), 'w') as f:
    #     f.write(str(os.getpid()) + '\n')
    log_file = os.path.join(tmp_dir, 'log')
    log_handler = logging.handlers.RotatingFileHandler(log_file,
                                                       backupCount = 1,
                                                       maxBytes = 4 * 1024 * 1024)
    log_handler.setLevel(logging.INFO)
    global log
    log = logging.getLogger()
    log.setLevel(logging.INFO)
    log.addHandler(log_handler)
    global common
    common = import_module('interp_python_common.py')
    global request_num
    request_num = 0
    global separator
    separator = '-' * 80
    # sys.exit()
    # Create server:
    server = SimpleXMLRPCServer(('localhost', 8000), requestHandler = RequestHandler)
    server.register_introspection_functions()
    server.register_function(execute)
    server.serve_forever() # run the server's main loop
    # with SimpleXMLRPCServer(('localhost', 8000),
    #                         requestHandler = RequestHandler) as server:
    #     server.register_introspection_functions()
    #     server.register_function(execute)
    #     server.serve_forever() # run the server's main loop

# main()
# sys.exit()

# with daemon.DaemonContext(stdout = open('/tmp/interp_python_log', 'w+')):
with daemon.DaemonContext():
    main()
