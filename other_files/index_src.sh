#!/bin/bash

# Reads files list from stdin (files are separated by \n).
# Creats CTAGS_FILE and CSCOPE_FILE.

usage()
{
    echo -e "Usage: $(basename $0) CTAGS_FILE [CTAGS_OPTIONS] -- CSCOPE_FILE [CSCOPE_OPTIONS]" 1>&2
    exit 1
}

STATE=CTAGS_FILE
while [ $# -ne 0 ]; do
    case $STATE in
        CTAGS_FILE)
            CTAGS_FILE="$1"
            STATE=CTAGS_OPTIONS
            ;;
        CTAGS_OPTIONS)
            if [ "$1" = "--" ]; then
                STATE=CSCOPE_FILE
                shift
                continue
            fi
            CTAGS_OPTIONS="$CTAGS_OPTIONS $1"
            ;;
        CSCOPE_FILE)
            CSCOPE_FILE="$1"
            STATE=CSCOPE_OPTIONS
            ;;
        CSCOPE_OPTIONS)
            CSCOPE_OPTIONS="$CSCOPE_OPTIONS $1"
            ;;
        *)
            usage
            ;;
    esac
    shift
done

if [ "$STATE" != "CSCOPE_OPTIONS" ]; then
    usage
fi

CTAGS_CMD="ctags-exuberant --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=no -L - -e $CTAGS_OPTIONS -f $CTAGS_FILE"
# -k     "Kernel  Mode", turns off the use of the default include dir
CSCOPE_CMD="cscope -b -i- $CSCOPE_OPTIONS -f $CSCOPE_FILE"

tee >($CTAGS_CMD) | $CSCOPE_CMD

# Run cscope in navigation mode (do not index anything):
# cscope -d -f $CSCOPE_FILE

# Create cross-reference tag for CCTree:
# ccglue -S cscope.out -o cctree.out
