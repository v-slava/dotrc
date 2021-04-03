#!/bin/bash

# For sway:
set -e
swaymsg -t get_inputs | python3 -c '\
import sys, json
inputs = json.load(sys.stdin)
for input in inputs:
    identifier = input["identifier"]
    if "touchpad" in identifier or "Touchpad" in identifier:
        send_events = input["libinput"]["send_events"]
        print(f"{identifier} {send_events}")
        break
' | while read TOUCHPAD_ID OLD_STATE ; do
    if [ "$OLD_STATE" = "disabled" ]; then
        NEW_STATE=enabled
    else
        NEW_STATE=disabled
    fi
    swaymsg input $TOUCHPAD_ID events $NEW_STATE 1>/dev/null
done
exit

# For i3:
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
if [ "$1" = "--init" ]; then
    xinput set-prop $TOUCHPAD_ID "libinput Natural Scrolling Enabled" 1
fi
xinput $ACTION $TOUCHPAD_ID
