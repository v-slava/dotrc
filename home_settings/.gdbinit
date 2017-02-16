python

import re
import subprocess

class SafeBreakpoint(gdb.Command):
    'Set a breakpoint. Terminate in case of failure. See also "help b".'

    safe_breakpoint_command = 'sb'

    def __init__(self):
        super(SafeBreakpoint, self).__init__(self.safe_breakpoint_command, gdb.COMMAND_USER)

    def is_pending(self, gdb_output):
        ret = re.search('Function ".*" not defined.', gdb_output)
        # Optionally GDB also prints:
        # Make breakpoint pending on future shared library load?
        return ret != None

    def invoke(self, arg, from_tty):
        # print('My safe breakpont: begin')
        # bp = gdb.Breakpoint(arg)
        # if (bp.pending):
        #     print('is pending')
        gdb_output = gdb.execute('b ' + arg, False, True)
        if (self.is_pending(gdb_output)):
            subprocess.check_call(['zenity', '--error', '--title', 'gdb script', '--text',
                "Can't set a breakpont: \"" + self.safe_breakpoint_command + ' ' + arg + '".'])

SafeBreakpoint()

# This is the end of python code:
end

# GDB code starts here:
# help break
