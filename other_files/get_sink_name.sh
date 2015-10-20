#!/bin/bash

USAGE="$(basename $0) [SINK]"

if [ $# -ge 1 ] ; then
	SINK="$1"
	shift
else
	if ~/os_settings/other_files/bluetooth_connected.sh ; then
		SINK="bluez_sink"
	else
		SINK="alsa_output"
	fi
fi

if [ $# -ne 0 ]; then
	echo -e "$USAGE" 2>&1
	exit 1
fi

# Get full sink name:
SINK_NAME=""
while [ "$SINK_NAME" = "" ]; do
	SINK_NAME="$(pactl list sinks | grep "Name: $SINK\." | cut -d' ' -f2)"
done

# Print full sink name:
echo "$SINK_NAME"

