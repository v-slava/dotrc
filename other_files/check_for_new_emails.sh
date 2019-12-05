#!/bin/bash

NOTIFICATIONS_TO_SHOW=3
MAIL=$HOME/mail
LOCK_DIR=/tmp/mbsync_lock_dir

set -e

if mkdir "$LOCK_DIR" 2>/dev/null ; then
    mbsync -a
    rm -rf "$LOCK_DIR"
fi

find $HOME/mail -maxdepth 1 -type d -not -path $HOME/mail \
        | while read ACCOUNT_DIR ; do
    INBOX=${ACCOUNT_DIR}/inbox
    if [ ! -d "$INBOX" ]; then
        continue
    fi
    NEW=$INBOX/new
    if [ ! -d "$NEW" ]; then
        continue
    fi
    for FILE in $(ls "$NEW") ; do
        if [ $NOTIFICATIONS_TO_SHOW -eq 0 ]; then
            exit 0
        fi
        NOTIFICATIONS_TO_SHOW=$((NOTIFICATIONS_TO_SHOW - 1))
        FILE_PATH=$NEW/$FILE
        FROM="$(grep '^From: ' "$FILE_PATH" | cut -d' ' -f2- | \
            sed -e 's|<|(|g' -e 's|>|)|g')"
        SUBJECT="$(grep '^Subject: ' "$FILE_PATH" | cut -d' ' -f2-)"
        # In 3 minutes (300 000 ms) we will check for new emails once again.
        notify-send -u low -t 280000 "Got new email from $FROM:
$SUBJECT"
    done
done
