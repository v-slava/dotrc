#!/bin/bash

set -e

USAGE="Usage: $(basename $0) [FILE] ..."
usage()
{
	echo -e "$USAGE" 1>&2
	exit 1
}

if [ $# -eq 0 ]; then
	usage
fi

for file in "$@" ; do
    filename=$(basename "$file")
    extension="${filename##*.}"
    case "$extension" in
        jpg | jpeg | png | bmp)
            gimp "$file" ;;
        *)
            echo "Error: \"$file\" unknown file type." 1>&2
            exit 1
            ;;
    esac
done
