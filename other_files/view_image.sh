#!/usr/bin/env bash

usage()
{
	echo "Usage: $(basename $0) IMAGE_FILE" 1>&2
	exit 1
}

set -e

if [ $# -ne 1 ]; then
	usage
fi

IMAGE_FILE="$1"

if pidof gliv 1>/dev/null ; then
	gliv -c "$IMAGE_FILE"
else
	nohup gliv "$IMAGE_FILE" 1>/dev/null 2>&1 &
fi

