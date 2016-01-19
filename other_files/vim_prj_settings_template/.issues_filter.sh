#!/usr/bin/env bash

# cat # no filter applied
# grep -v 'warning:'

# /home/user/workspace/my_prj/main.c:49:3: warning: Value stored to 'tmp' is never read

grep -v "/home/user/workspace/my_prj/main\.c:49:3: warning: Value stored to 'tmp' is never read" | grep -v ...

