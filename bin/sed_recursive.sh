#!/usr/bin/env bash

# Usage: $(basename $0) [-v] SED_ARGS ...
set -e

if [ "$1" = "-v" ]; then
    VERBOSE=true
    shift
fi

find -type f | while read FILE ; do
    if echo "$FILE" | grep -q '\.git' ; then
        continue
    fi
    if [ "$VERBOSE" = "true" ]; then
        echo "+ sed -i \"$FILE\" \"$@\""
    fi
    sed -i "$FILE" "$@"
done
