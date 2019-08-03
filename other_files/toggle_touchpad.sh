#!/bin/bash

TOUCHPAD_REGEXES=(
	"touchpad"
	"Synaptics TM2722-001"
)

for regex in "${TOUCHPAD_REGEXES[@]}" ; do
	TOUCHPAD_NAME="$(LANG=en xinput list --name-only | grep -i "$regex")"
	if [ -n "$TOUCHPAD_NAME" ]; then
		break
	fi
done
if [ -z "$TOUCHPAD_NAME" ]; then
	echo "touchpad's name was not found in input devices list" 1>&2
	exit 1
fi
TOUCHPAD_ID="$(LANG=en xinput list --id-only "$TOUCHPAD_NAME")"
VALUE=$(LANG=en xinput list-props $TOUCHPAD_ID | grep "Device Enabled" | cut -d: -f2 | cut -f2)
case "$VALUE" in
	0) ACTION=--enable ;;
	1) ACTION=--disable ;;
	*) echo "Error: got unexpected enabled VALUE = |$VALUE| (neither |0|, nor |1|)" 1>&2 ; exit 1 ;;
esac
xinput $ACTION $TOUCHPAD_ID
