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
if [ -n "$PID" ]; then
	imv-msg $PID "open $(realpath $IMAGE_FILE)"
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

