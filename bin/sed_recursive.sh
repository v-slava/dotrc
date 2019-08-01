#!/usr/bin/env bash

# Usage: $(basename $0) [-v] SED_ARGS ...
set -e

if [ "$1" = "-v" ]; then
    VERBOSE=true
    shift
fi

find -type f | while read file ; do
    if echo $file | grep -q '.git' ; then
        continue
    fi
    if [ "$VERBOSE" = "true" ]; then
        echo "+ sed -i \"$file\" $@"
    fi
    sed -i "$file" "$@"
done
