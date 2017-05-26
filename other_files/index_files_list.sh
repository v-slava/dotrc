#!/bin/bash

# Reads files list from stdin (files are separated by \n).
# Creats CTAGS_FILE and CSCOPE_FILE.

usage()
{
    echo -e "Usage: $(basename $0) [--ctags CTAGS_FILE [CTAGS_OPTIONS]] [--cscope CSCOPE_FILE [CSCOPE_OPTIONS]]" 1>&2
    exit 1
}

STATE=NONE
CTAGS_FILE=""
CTAGS_OPTIONS=""
CSCOPE_FILE=""
CSCOPE_OPTIONS=""

while [ $# -ne 0 ]; do
    case $1 in
        --ctags)
            if [ "$STATE" = "CTAGS_FILE" ] || [ -n "$CTAGS_FILE" ]; then
                usage
            fi
            STATE=CTAGS_FILE ;;
        --cscope)
            if [ "$STATE" = "CSCOPE_FILE" ] || [ -n "$CSCOPE_FILE" ]; then
                usage
            fi
            STATE=CSCOPE_FILE ;;
    esac
    case $1 in
        --ctags | --cscope) shift ; continue ;;
    esac

    case $STATE in
        NONE)
            usage
            ;;
        CTAGS_FILE)
            CTAGS_FILE="$1"
            STATE=CTAGS_OPTIONS
            ;;
        CTAGS_OPTIONS)
            CTAGS_OPTIONS="$CTAGS_OPTIONS $1"
            ;;
        CSCOPE_FILE)
            CSCOPE_FILE="$1"
            STATE=CSCOPE_OPTIONS
            ;;
        CSCOPE_OPTIONS)
            CSCOPE_OPTIONS="$CSCOPE_OPTIONS $1"
            ;;
    esac
    shift
done

if [ -n "$CTAGS_FILE" ]; then
    rm -f "$CTAGS_FILE"
fi

if [ -n "$CSCOPE_FILE" ]; then
    rm -f "$CSCOPE_FILE"
fi

CTAGS_CMD="ctags-exuberant --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=no -L - -e $CTAGS_OPTIONS -f $CTAGS_FILE"
# -k     "Kernel  Mode", turns off the use of the default include dir
CSCOPE_CMD="cscope -b -i- $CSCOPE_OPTIONS -f $CSCOPE_FILE"

if [ -n "$CTAGS_FILE" ]; then
    if [ -n "$CSCOPE_FILE" ]; then
        # Here sed is used to add quotes around each file name:
        tee >($CTAGS_CMD) | sed 's/\(.*\)/"\1"/g' | $CSCOPE_CMD
    else
        $CTAGS_CMD
    fi
else
    if [ -n "$CSCOPE_FILE" ]; then
        # Here sed is used to add quotes around each file name:
        sed 's/\(.*\)/"\1"/g' | $CSCOPE_CMD
    else
        usage
    fi
fi

# Run cscope in navigation mode (do not index anything):
# cscope -d -f $CSCOPE_FILE

# Create cross-reference tag for CCTree:
# ccglue -S cscope.out -o cctree.out
