#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Usage: $(basename $0) URL" 1>&2
    exit 1
fi

URL="$1"
FILE_NAME="$(basename $URL)"

set -e
ssh -p $SSH_RASPBERRY_PI wget -P downloads $URL
scp -P $SSH_RASPBERRY_PI:downloads/$FILE_NAME ~/downloads/
ssh -p $SSH_RASPBERRY_PI rm downloads/$FILE_NAME
