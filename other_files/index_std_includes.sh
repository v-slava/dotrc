#!/bin/bash

INCLUDE_PATH=/usr/include
TAGS_DIR=$DOTRC_S/emacs_projects/tags
CTAGS_FILE=$TAGS_DIR/std.TAGS
CSCOPE_FILE=$TAGS_DIR/std.cscope.out

set -e
rm -f $CTAGS_FILE $CSCOPE_FILE
mkdir -p $TAGS_DIR
find $INCLUDE_PATH -type f | $DOTRC/other_files/index_files_list.sh --ctags $CTAGS_FILE --cscope $CSCOPE_FILE
