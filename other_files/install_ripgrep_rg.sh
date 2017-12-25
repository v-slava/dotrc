#!/usr/bin/env bash

if [ ! -f "rg" ]; then
    echo "Error: wrong folder" 1>&2
    exit 1
fi

set -e
cp rg /usr/bin
cp rg.1 /usr/share/man/man1
cp complete/rg.bash-completion /etc/bash_completion.d/
