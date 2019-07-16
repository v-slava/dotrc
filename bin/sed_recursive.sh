#!/usr/bin/env bash

# Usage: $(basename $0) [--verbose] SED_ARGS ...
set -e

if [ "$1" = "--verbose" ]; then
    VERBOSE=true
    shift
fi

find -type f | while read file ; do
    if [ "$VERBOSE" = "true" ]; then
        echo "+ sed -i \"$file\" $@"
    fi
    sed -i "$file" "$@"
done
