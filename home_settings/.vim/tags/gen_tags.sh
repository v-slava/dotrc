#!/usr/bin/env bash

INCLUDE_PATH=/usr/include
CTAGS_FILE="$PWD/std.ctags"
CSCOPE_FILE="$PWD/std.cscope"
FILES_LIST=/tmp/std_tags_files_list

rm -f $CTAGS_FILE $CSCOPE_FILE $FILES_LIST
find /usr/include -type f > $FILES_LIST
ctags -R --c++-kinds=+p --fields=+iaS --extra=+q -f "$CTAGS_FILE" -L $FILES_LIST
cscope -R -b -k -f"$CSCOPE_FILE" -i$FILES_LIST

