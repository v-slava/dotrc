#!/usr/bin/env bash

# start downloading .torrent file from ~/downloads:
# bindsym $WIN+t exec --no-startup-id exec $START $DOTRC/other_files/torrent_download.sh

set -e

ORIGINAL_FILE="$(ls ~/downloads/*.torrent)"
NEW_FILE=/tmp/$(basename "$ORIGINAL_FILE")
mv "$ORIGINAL_FILE" "$NEW_FILE"
exec transmission-gtk "$NEW_FILE"
