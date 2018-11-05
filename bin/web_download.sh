#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Usage: $(basename $0) URL" 1>&2
    exit 1
fi

set -e
URL="$1"

if echo "$URL" | grep -q '&redirect=' ; then
    PARTS=(${URL//&redirect=/ })
    URL="${PARTS[1]}"
    FILE_NAME="$(basename $URL | sed 's/%255F/_/g' | sed 's/%252D/_-_/g')"
else
    FILE_NAME="$(basename $URL)"
fi

$PI_SSH wget -O "downloads/$FILE_NAME" $URL
scp -P $PI_PORT $PI_USR@$PI_HOST:downloads/"$FILE_NAME" ~/downloads/
$PI_SSH rm downloads/$FILE_NAME
