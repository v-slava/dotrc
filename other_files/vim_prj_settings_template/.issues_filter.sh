#!/usr/bin/env bash

# cat # no filter applied
# grep -v 'warning:'

# /home/user/workspace/my_prj/main.c:49:3: warning: Value stored to 'tmp' is never read

# grep -v "/home/user/workspace/my_prj/main\.c:49:3: warning: Value stored to 'tmp' is never read" | grep -v ...

END='[^.]\+\.\(c\|cpp\|h\|hpp\):[0-9]\+:[0-9]\+: warning'

grep    -e 'error' \
        -e "folder_1/$END"                                                     \
        -e "folder_2/subfolder_1/prefix_$END"                                  \

