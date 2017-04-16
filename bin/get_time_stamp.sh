#!/bin/bash

ARG="$1"
if [ -z "$ARG" ]; then
    echo "Usage: $(basename $0) FILE" 1>&2
    exit 1
fi
ls -l --time-style=+%s "$ARG" | cut -d' ' -f 6
