#!/bin/bash

strace -x -f -s 8192 -o ~/h/trace_file "$@"
