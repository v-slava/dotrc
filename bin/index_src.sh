#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) PROJECT_NAME [find_ARGS]" 1>&2
	exit 1
}

if [ $# -lt 1 ]; then
	usage
fi
PROJECT_NAME="$1"
shift
TAGS_DIR=~/workspace/dotrc_s/emacs_projects/tags
CTAGS_FILE=$TAGS_DIR/$PROJECT_NAME.TAGS
CSCOPE_FILE=$TAGS_DIR/$PROJECT_NAME.cscope.out

~/os_settings/other_files/find_src.sh "$@" | ~/os_settings/other_files/index_files_list.sh $CTAGS_FILE -- $CSCOPE_FILE
