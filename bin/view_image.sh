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

if ! pidof gliv ; then
	gliv "$IMAGE_FILE" &
else
	gliv -c "$IMAGE_FILE"
fi

