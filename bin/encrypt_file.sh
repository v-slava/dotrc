#!/bin/bash

set -e

USAGE="Usage: $(basename $0) IN OUT"
usage()
{
    echo -e "$USAGE" 1>&2
    exit 1
}

if [ $# -ne 2 ]; then
    usage
fi

IN="$1"
OUT="$2"

if [ ! -f "$IN" ]; then
    echo "File \"$IN\" not found" 1>&2
    usage
fi

gpg --symmetric --cipher-algo aes256 -o "$OUT" < "$IN"
exit




# man openssl enc

IN_FILE=in_file
ENCRYPTED_FILE=encrypted_${IN_FILE}
DECRYPTED_FILE=decrypted_${IN_FILE}

if [[ -f $ENCRYPTED_FILE ]]; then
    rm $ENCRYPTED_FILE
fi
if [[ -f $DECRYPTED_FILE ]]; then
    rm $DECRYPTED_FILE
fi

KEY=$(openssl rand -hex 16)
IV=$(openssl rand -hex 16)

openssl enc    -aes-128-ctr -in $IN_FILE        -out $ENCRYPTED_FILE -K $KEY -iv $IV -p
openssl enc -d -aes-128-ctr -in $ENCRYPTED_FILE -out $DECRYPTED_FILE -K $KEY -iv $IV -p
