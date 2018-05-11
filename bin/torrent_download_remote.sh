#!/bin/bash

set -e

echo "Copy-paste URL and press ENTER:"
read URL

ssh -p $SSH_RASPBERRY_PI transmission-remote -a "\"$URL\"" \
    --torrent-done-script /home/pi/notify_torrent_complete.sh \
    --download-dir /home/pi/downloads

torrent_status.sh
