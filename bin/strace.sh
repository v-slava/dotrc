#!/bin/bash

strace -x -f -s 8192 -o ~/my/trace_file "$@"

