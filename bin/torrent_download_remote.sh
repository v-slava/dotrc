#!/bin/bash

set -e

echo "Copy-paste URL and press ENTER (if you have torrent file, just press ENTER):"
read URL

if [ -z "$URL" ]; then
    FILE=$(basename $(ls ~/downloads/*.torrent))
    scp -P $PI_PORT ~/downloads/$FILE $PI_USR@$PI_HOST:/home/$PI_USR/downloads/
    URL=/home/$PI_USR/downloads/$FILE
fi

$PI_SSH transmission-remote -a "\"$URL\"" \
    --torrent-done-script /home/$PI_USR/notify_torrent_complete.sh \
    --uplimit 0 --download-dir /home/$PI_USR/downloads

if [ -n "$FILE" ]; then
    $PI_SSH rm /home/$PI_USR/downloads/*.torrent
    rm ~/downloads/$FILE
fi

torrent_status.sh
