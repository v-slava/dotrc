#!/usr/bin/env bash

die() {
    echo "$1" >&2
    exit 1
}

# check if the script is run from vimb with socket support enabled
if [ -z "$VIMB_SOCKET" ] || [ ! -S "$VIMB_SOCKET" ]; then
    # die 'This script must be run from vimb with socket support'
	echo "no socket"
fi

REQUEST=$(clipboard.sh -o | sed 's/ /+/g')
echo ":o http://www.google.com/search?q=$REQUEST<CR>" | socat - unix-connect:"$VIMB_SOCKET"

