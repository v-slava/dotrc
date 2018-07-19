#!/usr/bin/env bash

set -e

ROOT_DIR=/tmp/python
rm -rf $ROOT_DIR
mkdir $ROOT_DIR
IN=$ROOT_DIR/in
OUT=$ROOT_DIR/out
ERR=$ROOT_DIR/err

mkfifo $IN

python3 -i -q $DOTRC/other_files/python/startup.py < $IN > $OUT 2>$ERR &
while true; do sleep 999999999; done > $IN &
# get next line from stdout:
# tail -n +$(($(cat /tmp/python/out | wc -l)+1)) -f /tmp/python/out | head -n 1
