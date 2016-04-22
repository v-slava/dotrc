#!/bin/bash

strace -f -s 8192 -o ~/trace_file "$@"

