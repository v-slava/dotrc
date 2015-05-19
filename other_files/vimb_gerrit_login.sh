#!/usr/bin/env bash

die() {
    echo "$1" >&2
    exit 1
}

if [ -z "$@" ]; then
	die "Usage: $(basename $0) GERRIT_URL"
fi

# check if the script is run from vimb with socket support enabled
if [ -z "$VIMB_SOCKET" ] || [ ! -S "$VIMB_SOCKET" ]; then
    die 'This script must be run from vimb with socket support'
fi

~/os_settings/other_files/vimb_formfiller.sh "$@"
# sign in to gerrit:
echo "fsi" | socat - unix-connect:"$VIMB_SOCKET"

