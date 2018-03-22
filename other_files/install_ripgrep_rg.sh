#!/usr/bin/env bash

if [ ! -f "rg" ]; then
    echo "Error: wrong folder" 1>&2
    exit 1
fi

set -ex
cp rg /usr/bin
cp doc/rg.1 /usr/share/man/man1
cp complete/rg.bash /etc/bash_completion.d/
