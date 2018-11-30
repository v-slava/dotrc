#!/bin/bash

set -e

USAGE="Usage: $(basename $0) ELF_FILE"

usage()
{
	echo -e "$USAGE" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi

ELF_FILE="$1"

${MY_CROSS_COMPILE}nm --defined-only "$ELF_FILE" | sed -e '/ \$[dt]$/d' | cut -d' ' -f3 | ${MY_CROSS_COMPILE}c++filt | sort

