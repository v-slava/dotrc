#!/bin/bash

set -e

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
SINK_NAME="@DEFAULT_SINK@"

# amixer set Master 3%+
pactl -- set-sink-volume "$SINK_NAME" "$VOLUME"
exit

# Set the same volume for all applications:
NEW_VOLUME=$($DOTRC/other_files/get_volume.sh)
SINK_INPUT_NUMBERS=$(LANGUAGE=en pactl list sink-inputs | grep 'Sink Input #' | cut -d'#' -f2)
for sink_input_number in $SINK_INPUT_NUMBERS ; do
    pactl set-sink-input-volume "$sink_input_number" "$NEW_VOLUME"
done
