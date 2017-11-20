#!/bin/bash

usage()
{
    echo -e "Usage: $(basename $0) PROJECT_NAME ARCH [--ctags CTAGS_OPTIONS] [--cscope CSCOPE_OPTIONS]" 1>&2
    exit 1
}

if [ $# -lt 2 ]; then
    usage
fi
PROJECT_NAME="$1"
shift
ARCH="$1"
shift
TAGS_DIR=~/workspace/dotrc_s/emacs_projects/tags
CTAGS_FILE=$TAGS_DIR/$PROJECT_NAME.TAGS
CSCOPE_FILE=$TAGS_DIR/$PROJECT_NAME.cscope.out

STATE=NONE
CTAGS_OPTIONS=""
CSCOPE_OPTIONS=""

while [ $# -ne 0 ]; do
    case $1 in
        --ctags)  STATE=CTAGS_OPTIONS  ;;
        --cscope) STATE=CSCOPE_OPTIONS ;;
    esac
    case $1 in
        --ctags | --cscope) shift ; continue ;;
    esac

    case $STATE in
        NONE)           usage                               ;;
        CTAGS_OPTIONS)  CTAGS_OPTIONS="$CTAGS_OPTIONS $1"   ;;
        CSCOPE_OPTIONS) CSCOPE_OPTIONS="$CSCOPE_OPTIONS $1" ;;
    esac
    shift
done

(~/os_settings/other_files/find_src.sh -path $(realpath $PWD/arch) -prune -o && cd arch/$ARCH && ~/os_settings/other_files/find_src.sh) | ~/os_settings/other_files/index_files_list.sh --ctags "$CTAGS_FILE" $CTAGS_OPTIONS --cscope "$CSCOPE_FILE" $CSCOPE_OPTIONS
