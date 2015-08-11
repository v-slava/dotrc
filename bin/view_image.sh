#!/usr/bin/env bash

usage()
{
	echo "Usage: $(basename $0) IMAGE_FILE" 1>&2
	exit 1
}
echo asdf > ~/asdf

set -e

if [ $# -ne 1 ]; then
	usage
fi

IMAGE_FILE="$1"

if ! pidof gliv ; then
	gliv &
	sleep 1
fi

gliv -c "$IMAGE_FILE"

