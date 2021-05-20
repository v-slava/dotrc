#!/bin/bash

set -e

if [ "$1" = "--startup" ]; then
    while ! ping -c 1 -W 1 8.8.8.8 ; do
        echo "Waiting for 8.8.8.8 (network interface might be down)..."
        sleep 1
    done
fi

# evolution
thunderbird
