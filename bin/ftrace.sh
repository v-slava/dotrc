#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Usage: $(basename $0) TRACE_FILE"
    exit 1
fi

set -e
FILE="$1"
sed -i "$FILE" -e 's/^ 0)/ 0|/g'
sed -i "$FILE" -e 's/^ 1)/ 1|/g'
