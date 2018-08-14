#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Usage: $(basename $0) URL" 1>&2
    exit 1
fi

URL="$1"
FILE_NAME="$(basename $URL)"

set -e
$PI_SSH wget -P downloads $URL
scp -P $PI_PORT $PI_USR@$PI_HOST:downloads/$FILE_NAME ~/downloads/
$PI_SSH rm downloads/$FILE_NAME
