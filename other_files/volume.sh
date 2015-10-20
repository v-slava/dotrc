#!/usr/bin/env bash

USAGE="Usage:\n\n\
$(basename $0) VOLUME\n\n\
VOLUME must be in pactl(1) format. VOLUME examples:\n20%\n+3%\n-5%"

if [ -z "$1" ] ; then
	echo -e "$USAGE" 1>&2
	exit 1
fi

VOLUME="$1"
SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"

# amixer set Master 3%+
pactl -- set-sink-volume "$SINK_NAME" "$VOLUME"

