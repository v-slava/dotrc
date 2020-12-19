#!/bin/bash

DB=/media/files/for_backup/phone/keepass/keepass.kdbx
ENTRY='banks/OTP Bank'
TIMEOUT=15

set -e

OUTPUT="$(keepassxc-cli show $DB "$ENTRY")"
LINE_START=$(echo "$OUTPUT" | grep -n 'OTP smart' | cut -d: -f1)
TAIL="$(echo "$OUTPUT" | tail -n +$LINE_START)"
LINE_END="$(echo "$TAIL" | grep -n '^$' | head -n 1 | cut -d: -f1)"
TEXT="$(echo "$TAIL" | head -n $LINE_END)"
echo "$TEXT" | grep -i pass | cut -d' ' -f2 | clipboard.sh -n
echo "Password copied to clipboard for $TIMEOUT seconds"
bash -c "sleep $TIMEOUT ; echo -n '' | clipboard.sh -n" &
