#!/bin/bash

strace -x -f -s 8192 -o ~/trace_file "$@"

