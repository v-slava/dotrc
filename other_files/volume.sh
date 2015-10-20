#!/usr/bin/env bash

USAGE="Usage:\n\n\
$(basename $0) VOLUME\n\n\
VOLUME must be in pactl(1) format. VOLUME examples:\n20%\n+3%\n-5%"

if [ -z "$1" ] ; then
	echo -e "$USAGE" 1>&2
	exit 1
fi

VOLUME="$1"

source ~/.config_xdg/BT_MAC.sh

if ~/os_settings/other_files/bluetooth_connected.sh ; then
	SINK="$(pactl list sinks | grep 'Name: bluez_sink\.' | cut -d' ' -f2)"
else
	SINK="$(pactl list sinks | grep 'Name: alsa_output' | cut -d' ' -f2)"
fi

# amixer set Master 3%+
pactl -- set-sink-volume "$SINK" "$VOLUME"

