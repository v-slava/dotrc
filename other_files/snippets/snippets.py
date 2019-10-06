class temp_shell_script:
    def __init__(self, cmd):
        with tempfile.NamedTemporaryFile('w', delete = False,
                prefix = 'bear_cmd_wrapper_') as f:
            self.name = f.name
            f.write('#!/bin/bash\n\n{}\n'.format(cmd))
        import stat
        st = os.stat(self.name)
        os.chmod(self.name, st.st_mode | stat.S_IEXEC)
    def _remove(self):
        if not self.name:
            os.remove(self.name)
            self.name = None
    def __del__(self):
        try:
            self._remove()
        except:
            pass
    def __enter__(self):
        if not self.name:
            raise ValueError("Shell script name is not initialized.")
        # os.stat(self.name) # throws exception if file doesn't exist
        return self
    def __exit__(self, type, value, traceback):
        self._remove()

with temp_shell_script(cmd) as s:
    return run_build(s.name, *args, **kwargs)
