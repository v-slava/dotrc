#!/usr/bin/env bash

# Disassemble FILE or a single INSTRUCTION and print asm instructions to stdout.
# System uses EL (little endian), but objdump produces EB.
# ARM architecture reference manual also uses EB.

usage()
{
	echo "Usage $(basename $0) {-a|-t} {-EB|-EL} {FILE|INSTRUCTION_EL}" 1>&2
	exit 1
}

if [ $# -ne 3 ]; then
    usage
fi

if [ "$1" = "-t" ]; then
	THUMB="-Mforce-thumb"
else
    if [ "$1" != "-a" ]; then
        usage
    fi
fi
if [ "$2" != "-EB" ] && [ "$2" != "-EL" ]; then
    usage
fi
ENDIANESS="$2"

TMP_FILE=/tmp/disassemble_sh_tmp
set -e
rm -f "$TMP_FILE"

if [ -f "$3" ]; then
	IN_FILE="$3"
else
	IN_FILE=/tmp/disassemble_sh_in
	echo "$3" > "$IN_FILE"
fi

xxd -r -p "$IN_FILE" "$TMP_FILE"
${CROSS_COMPILE}objdump -D -b binary $ENDIANESS -marm $THUMB "$TMP_FILE" | tail -n +8

