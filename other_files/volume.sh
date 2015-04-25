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
	SINK="bluez_sink.$(echo $BT_MAC | sed 's/:/_/g')"
else
	SINK="alsa_output.pci-0000_00_1b.0.analog-stereo"
fi

# amixer set Master 3%+
pactl -- set-sink-volume "$SINK" "$VOLUME"

