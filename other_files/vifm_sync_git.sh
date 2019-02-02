#!/bin/bash

# %c current file name
# %C other file full path
# %D other directory full path

if [ $# -ne 3 ]; then
    echo "Usage: $(basename $0) %c %C %D" 1>&2
    exit 1
fi

c="$1"
C="$2"
D="$3"

GIT_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)
RET=$?
if [ $RET -ne 0 ]; then
    echo "$PWD is not in git repository" 1>&2
    exit $RET
fi

TO_SELECT_RELATIVE="$(realpath --relative-to="$GIT_ROOT" "$c")"
TO_SELECT_ABSOLUTE="$D/$TO_SELECT_RELATIVE"

if [ ! -e "$TO_SELECT_ABSOLUTE" ]; then
    echo "Item to select doesn't exist in directory \"$D\"." 1>&2
    echo "Requested: \"$TO_SELECT_RELATIVE\"" 1>&2
    RET=1
    while : ; do
        TO_SELECT_ABSOLUTE="$(dirname "$TO_SELECT_ABSOLUTE")"
        [ -e "$TO_SELECT_ABSOLUTE" ] && break
    done
    TO_SELECT_RELATIVE="$(realpath --relative-to="$D" "$TO_SELECT_ABSOLUTE")"
    echo "Available: \"$TO_SELECT_RELATIVE\"" 1>&2
    if [ "$TO_SELECT_RELATIVE" = "." ]; then
        echo "Exiting." 1>&2
        exit $RET
    else
        echo "Choosing the one available." 1>&2
    fi
fi

TO_SELECT_DIR="$(dirname "$TO_SELECT_ABSOLUTE")"
TO_SELECT_FILE="$(basename "$TO_SELECT_ABSOLUTE")"

vifm --server-name $VIFM_SERVER_NAME --remote -c "mark 0 '$TO_SELECT_DIR' '$TO_SELECT_FILE' | wincmd w"
vifm --server-name $VIFM_SERVER_NAME --remote -c "normal '0 | wincmd w"
exit $RET
