#!/bin/bash

set -e

echo "Copy-paste URL and press ENTER (if you have torrent file, just press ENTER):"
read URL

if [ -z "$URL" ]; then
    FILE=$(basename $(ls ~/downloads/*.torrent))
    scp -P 53535 ~/downloads/$FILE pi@94.154.220.9:/home/pi/downloads/
    URL=/home/pi/downloads/$FILE
fi

ssh -p $SSH_RASPBERRY_PI transmission-remote -a "\"$URL\"" \
    --torrent-done-script /home/pi/notify_torrent_complete.sh \
    --uplimit 0 --download-dir /home/pi/downloads

if [ -n "$FILE" ]; then
    ssh -p $SSH_RASPBERRY_PI rm /home/pi/downloads/*.torrent
    rm ~/downloads/$FILE
fi

torrent_status.sh
