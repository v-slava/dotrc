#!/usr/bin/env bash

# Overwrites HEX_DATA in FILE starting from HEX_OFFSET

if [ $# -ne 3 ]; then
	echo -e "Usage: $(basename $0) FILE HEX_OFFSET HEX_DATA\n\
For example: \"$(basename $0) some_file 1234 56789A\" writes array {0x56, 0x78, 0x9A} starting from offset 0x1234" 1>&2
	exit 1
fi

OUT_FILE="$1"
HEX_OFFSET="$2"
HEX_DATA="$3"

set -e

DEC_OFFSET=`printf %d 0x$HEX_OFFSET`
echo "$HEX_DATA" | xxd -r -p | dd of="$OUT_FILE" obs=1 seek="$DEC_OFFSET" conv=notrunc,nocreat status=none

