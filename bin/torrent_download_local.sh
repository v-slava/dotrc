#!/bin/bash

# scp -r -P $PI_PORT $PI_USR@$PI_HOST:downloads/* ~/downloads/

REMOTE=/home/$PI_USR/torrent_parts
LOCAL=$HOME/downloads/torrent
TMP=$LOCAL/tmp

set -e

if [ ! -d "$LOCAL" ]; then
    mkdir -p "$LOCAL"
    mkdir -p "$TMP"
    cat << EOF | $PI_SSH
set -e
transmission-remote -t all -r
rm -rf "$REMOTE"
mkdir -p "$REMOTE"
cd "$REMOTE"
tar c -C ~/downloads/ . | split -b 10M
shopt -s dotglob
sudo rm -rf ~/downloads/*
EOF
fi

FILE="$($PI_SSH "set -o pipefail && ls $REMOTE | head -n 1")"
while [ -n "$FILE" ] ; do
    rm -f "$LOCAL/$FILE" "$TMP/$FILE"
    scp -P $PI_PORT $PI_USR@$PI_HOST:$REMOTE/$FILE "$TMP/$FILE"
    mv "$TMP/$FILE" "$LOCAL/$FILE"
    FILE="$($PI_SSH "rm $REMOTE/$FILE && set -o pipefail && ls $REMOTE \
        | head -n 1")"
done

set -o pipefail
cat $LOCAL/* | tar x -C ~/downloads
rm -rf "$TMP" "$LOCAL"
