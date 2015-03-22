#!/usr/bin/env bash

# Assemble FILE or a single INSTRUCTION and print hex strings to stdout
# in big endian format (exactly as it is in ARM reference manual).

if [ $# -ne 2 ]; then
	echo "Usage: $(basename $0) {-a|-t} {FILE|INSTRUCTION}" 1>&2
	exit 1
fi

set -e

ROOT_FOLDER=/tmp/assemble_sh_files
if [ ! -d $ROOT_FOLDER ]; then
	mkdir $ROOT_FOLDER
fi

if [ "$1" = "-a" ]; then
	echo ".code 32" > $ROOT_FOLDER/code.s
else
	echo ".code 16" > $ROOT_FOLDER/code.s
fi
if [ -f "$2" ]; then
	cat "$2" >> $ROOT_FOLDER/code.s
else
	echo "$2" >> $ROOT_FOLDER/code.s
fi
arm-linux-gnueabi-as $ROOT_FOLDER/code.s -o $ROOT_FOLDER/code.o
arm-linux-gnueabi-objcopy -O binary $ROOT_FOLDER/code.o $ROOT_FOLDER/code.bin
if [ "$1" = "-a" ]; then
	#hexdump -v -e '/4 "%08_ax:   "' -e '/4 "%08x\n"' $ROOT_FOLDER/code.bin
	hexdump -v -e '/4 "%08x\n"' $ROOT_FOLDER/code.bin
else
	#hexdump -v -e '/2 "%08_ax:   "' -e '/2 "%04x\n"' $ROOT_FOLDER/code.bin
	hexdump -v -e '/2 "%04x\n"' $ROOT_FOLDER/code.bin
fi

