#!/usr/bin/env bash

# Assemble and disassemble INSTRUCTION

if [ $# -ne 2 ]; then
	echo "Usage: $(basename $0) {-a|-t} INSTRUCTION" 1>&2
	exit 1
fi
set -e
disassemble.sh "$1" -EB `assemble.sh "$1" "$2"`

