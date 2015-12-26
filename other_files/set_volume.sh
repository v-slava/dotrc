#!/bin/bash

USAGE="Usage: $(basename $0) VOLUME

VOLUME must be in pactl(1) format. VOLUME examples:
20%
+3%
-5%
"

if [ -z "$1" ] ; then
	echo -e "$USAGE" 1>&2
	exit 1
fi

VOLUME="$1"
SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"

# amixer set Master 3%+
pactl -- set-sink-volume "$SINK_NAME" "$VOLUME"

