#!/bin/bash

FULL_PATH=/media/files/permanent/music
BASE_NAME=$(basename $FULL_PATH)

tar cf ~/h/music.tar -C $FULL_PATH/.. --exclude='*.sh' \
    --exclude="$BASE_NAME/no_words/sleep/video" $BASE_NAME
