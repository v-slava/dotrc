#!/usr/bin/env bash

# Disassemble FILE or a single INSTRUCTION and print asm instructions to stdout.
# System uses EL (little endian), but objdump produces EB.
# ARM architecture reference manual also uses EB.

if [ $# -ne 3 ]; then
	echo "Usage $(basename $0) {-a|-t} {-EB|-EL} {FILE|INSTRUCTION}" 1>&2
	exit 1
fi

if [ "$1" = "-t" ]; then
	THUMB=" -Mforce-thumb "
fi

TMP_FILE=/tmp/disassemble_sh_tmp
set -e
if [ -f "$TMP_FILE" ]; then
	rm "$TMP_FILE"
fi

if [ -f "$3" ]; then
	IN_FILE="$3"
else
	IN_FILE=/tmp/disassemble_sh_in
	echo "$3" > "$IN_FILE"
fi

xxd -r -p "$IN_FILE" "$TMP_FILE"
${CROSS_COMPILE}objdump -D -b binary "$2" -marm "$THUMB" "$TMP_FILE" | tail -n +8

