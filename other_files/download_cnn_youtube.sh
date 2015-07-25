#!/usr/bin/env bash

set -e

if [ $# -ne 1 ]; then
	echo "Usage: $(basename $0) YouTube_URL" 1>&2
	exit 1
fi
URL="$1"
# https://www.youtube.com/watch?v=4qqCtliBupA&list=PLpCR8mo8DkOzwhhTg7aO1B1TzikBA4YGa&index=9

youtube-dl --write-sub "$URL"
LAST_NUMBER=$(ls cnn_*.mp4 | tail -n 1 | cut -d'.' -f1 | cut -c 5-)
let CUR_NUMBER=$LAST_NUMBER+1
rename "s/CNN Student News[^.]*\./cnn_$CUR_NUMBER\./" *

