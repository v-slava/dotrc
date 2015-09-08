#!/bin/bash

set -e

USAGE="Usage: $(basename $0) IN OUT"
usage()
{
	echo -e "$USAGE" 1>&2
	exit 1
}

if [ $# -ne 2 ]; then
	usage
fi

IN="$1"
OUT="$2"

if [ ! -f "$IN" ]; then
	echo "File \"$IN\" not found" 1>&2
	usage
fi

gpg --symmetric --cipher-algo aes256 -o "$OUT" < "$IN"

