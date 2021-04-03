#!/usr/bin/env bash

if [ $# -ne 2 ]; then
    echo "Usage: $(basename $0) FROM TO" 1>&2
    exit 1
fi

FROM="$1"
TO="$2"
i3-msg "workspace $FROM, move container to workspace $TO"
