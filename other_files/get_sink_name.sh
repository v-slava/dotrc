#!/bin/bash

USAGE="$(basename $0) [SINK]"

if [ $# -ge 1 ] ; then
	SINK="$1"
	shift
else
	if $DOTRC/other_files/bluetooth_connected.sh ; then
		SINK="bluez_sink\."
	else
		# SINK="alsa_output\."
		# SINK="alsa_output\..*\.hdmi-stereo$"
		SINK="alsa_output\..*\.analog-stereo$"
	fi
fi

if [ $# -ne 0 ]; then
	echo -e "$USAGE" 2>&1
	exit 1
fi

# Get full sink name:
SINK_NAME=""
ITERATION_NUM=0
while [ "$SINK_NAME" = "" ]; do
	SINK_NAME="$(LANGUAGE=en pactl list sinks | grep "Name: $SINK" | cut -d' ' -f2)"
	((ITERATION_NUM++))
	if [ $ITERATION_NUM -gt 300 ]; then
		echo "Failed to get full sink name: need to increase number of iterations." 1>&2
		exit 1
	fi
done

# Print full sink name:
echo "$SINK_NAME"

