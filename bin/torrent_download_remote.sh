#!/bin/bash

set -e

ssh -p $SSH_RASPBERRY_PI transmission-remote -a -f /home/pi/notify_torrent_complete.sh "$URL"
