#!/bin/bash

set -e

DIR="$(dirname $(realpath $0))"

read IN

ARTIST=$(echo $IN | cut -d'-' -f1 | "$DIR/vifm_rename_to_lowercase.sh")
ARTIST_LEN=${#ARTIST}
TRACK_NAME_START=$(echo ${IN:$((ARTIST_LEN + 1)):${ARTIST_LEN}} | "$DIR/vifm_rename_to_lowercase.sh")

if [ "$ARTIST" = "$TRACK_NAME_START" ]; then
	NO_START="spaces"
	TRACK_NAME_START="${IN:$((ARTIST_LEN * 2 + 2)):${#NO_START}}"
	if [ "$TRACK_NAME_START" != "$NO_START" ]; then
		# Skip extra artist name:
		IN="${ARTIST}-${IN:$((ARTIST_LEN * 2 + 2))}"
	fi
fi

echo "$IN" | sed 's/-spaces.ru//g' | sed 's/-/_-_/g' | "$DIR/vifm_rename_spaces_to_underscores.sh" | "$DIR/vifm_rename_to_lowercase.sh"
