#!/usr/bin/env bash

if [ $# -ne 1 ]; then
	echo "Usage: $(basename $0) WORD_TO_SAY" 1>&2
	exit 1
fi

WORD_TO_SAY="$1"
FILE_NAME="${WORD_TO_SAY}.mp3"
DIR=/tmp
FILE_PATH="$DIR/$FILE_NAME"
SOUNDS=~/other/GoldenDict/sound_en/sound_en.dsl.files.zip

set -e

if [ ! -f "$FILE_PATH" ]; then
	unzip $SOUNDS "$FILE_NAME" -d "$DIR"
fi
mplayer "$FILE_PATH"
# mplayer --volume=70 "$FILE_PATH"

