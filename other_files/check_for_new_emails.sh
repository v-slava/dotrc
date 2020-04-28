#!/bin/bash

NOTIFICATIONS_TO_SHOW=3
MAIL=$HOME/mail
# LOCK_DIR=/tmp/mbsync_lock_dir

PYTHON_SCRIPT="import sys, email; from email import header
data = email.header.decode_header(sys.stdin.read())[0][0]
try:
    data = data.decode()
except (UnicodeDecodeError, AttributeError):
    pass
print(data)"

set -e

# if mkdir "$LOCK_DIR" 2>/dev/null ; then
    mbsync -a
#     rm -rf "$LOCK_DIR"
# fi

find $MAIL -maxdepth 1 -type d -not -path $MAIL | while read ACCOUNT_DIR ; do
    NEW=${ACCOUNT_DIR}/inbox/new
    if [ ! -d "$NEW" ]; then
        continue
    fi
    for FILE in $(ls "$NEW") ; do
        if [ $NOTIFICATIONS_TO_SHOW -eq 0 ]; then
            exit 0
        fi
        NOTIFICATIONS_TO_SHOW=$((NOTIFICATIONS_TO_SHOW - 1))
        FILE_PATH=$NEW/$FILE
        FROM="$(grep '^From: ' "$FILE_PATH" | cut -d' ' -f2-)"
        SUBJECT="$(grep '^Subject: ' "$FILE_PATH" | cut -d' ' -f2-)"
        DECODED_SUBJECT="$(echo "$SUBJECT" | python3 -c "$PYTHON_SCRIPT")"
        # In 3 minutes (300 000 ms) we will check for new emails once again.
        notify-send -u low -t 280000 "Got new email from $FROM:
$DECODED_SUBJECT"
        # To mark new email as "seen":
        # mv inbox/new/${EMAIL_FILE} inbox/cur/${EMAIL_FILE}S
    done
done
