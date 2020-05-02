#!/bin/bash

# Print lines that are longer than MAX_COLUMNS. Without "-r" reads from stdin.

TAB_LEN=8
MAX_COLUMNS=80

usage()
{
    echo "Usage: $(basename $0) [-t|--tab-len TAB_LEN] [-r|--recursive]" 1>&2
    exit 1
}

set -e
OPTIONS=$(getopt -o t:r -l tab-len:,recursive -- "$@")
if [ $? -ne 0 ]; then
    usage
fi
eval set -- "$OPTIONS"
while true; do
    case "$1" in
        "-t" | "--tab-len")
            shift
            TAB_LEN="$1"
            ;;
        "-r" | "--recursive")
            RECURSIVE=true
            ;;
        '--')
            shift
            if [ $# -ge 1 ]; then
                echo "Error: unrecognized options found: $@" 2>&1
                usage
            fi
            break
            ;;
    esac
    shift
done

TAB="$(printf %${TAB_LEN}s)"
GREP_ARG=".\{$((MAX_COLUMNS + 1))\}"

if [ "$RECURSIVE" = "true" ]; then
    # Read all files recursively:
    find -type f | while read FILE ; do
        if echo "$FILE" | grep -q '\.git' ; then
            continue
        fi
        if cat "$FILE" | sed -e "s/\t/${TAB}/g" | grep -nq "$GREP_ARG" ; then
            echo "$FILE"
            cat "$FILE" | sed -e "s/\t/${TAB}/g" | grep -n "$GREP_ARG"
        fi
    done
else
    # Read from stdin:
    sed -e "s/\t/${TAB}/g" | grep -n "$GREP_ARG"
fi
