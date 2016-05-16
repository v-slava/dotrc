#!/bin/bash

set -e

USAGE="Usage: $(basename $0) {--wav|--mp3|--ogg} FILE

Note: FILE should be without extension.

Set volume to 100% when recording mp3.
Otherwise output volume will be low.
"

usage()
{
	echo -e "$USAGE"
	exit 1
}

if [ $# -ne 2 ] ; then
	usage
fi

SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"
FILE="$2"

case "$1" in
	("--wav") parec -d "$SINK_NAME.monitor" --file-format=wav "${FILE}.wav" ;;
	("--mp3") parec -d "$SINK_NAME.monitor" | lame -r - "${FILE}.mp3" ;; # --cbr -b 320
	("--ogg") parec -d "$SINK_NAME.monitor" | oggenc -b 192 -o "${FILE}.ogg" --raw - ;;
	(*) usage ;;
esac

