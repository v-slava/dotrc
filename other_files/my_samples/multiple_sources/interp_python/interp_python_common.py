class ExecutionContext:
    def __init__(self, succeeded, stdout, stderr):
        self.succeeded = succeeded
        self.stdout = stdout
        self.stderr = stderr
    def __repr__(self):
        return """succeeded = {}
stdout = |{}|
stderr = |{}|
""".format(self.succeeded, self.stdout, self.stderr)
