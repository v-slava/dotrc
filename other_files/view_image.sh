#!/usr/bin/env bash

usage()
{
	echo "Usage: $(basename $0) IMAGE_FILE" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi

IMAGE_FILE="$1"

PID=$(pidof imv-wayland)
set -e

TMP_FILE=/tmp/cur_viewed_image
update_tmp_file()
{
	EXTENSION=$(echo "$IMAGE_FILE" | rev | cut -d'.' -f1 | rev)
	TMP_FILE=$TMP_FILE.$EXTENSION
}

rm -f $TMP_FILE.*
if file "$IMAGE_FILE" | grep 'orientation=' | \
		grep -q -v 'orientation=upper-left' ; then
	update_tmp_file
	# convert -rotate 90 "$IMAGE_FILE" "$TMP_FILE"
	convert -auto-orient "$IMAGE_FILE" "$TMP_FILE"
	IMAGE_FILE="$TMP_FILE"
else
	# imv-msg requires full image path without spaces...
	IMAGE_FILE="$(realpath "$IMAGE_FILE")"
	if echo "$IMAGE_FILE" | grep -q ' ' ; then
		update_tmp_file
		ln -s "$IMAGE_FILE" $TMP_FILE
		IMAGE_FILE=$TMP_FILE
	fi
fi

if [ -n "$PID" ]; then
	imv-msg $PID "open $IMAGE_FILE"
	imv-msg $PID "close 1"
else
	imv-wayland -b 101010 "$IMAGE_FILE" &
fi
exit

if pidof gliv 1>/dev/null ; then
	gliv -c "$IMAGE_FILE"
else
	nohup gliv "$IMAGE_FILE" 1>/dev/null 2>&1 &
fi

