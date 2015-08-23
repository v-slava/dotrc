#!/bin/bash

set -e

TOUCHPAD_ID=$(xinput list | grep -i touchpad | cut -d'=' -f2 | cut -f1)
NAME="Device Enabled"
VALUE=$(xinput list-props $TOUCHPAD_ID | grep "$NAME" | cut -d: -f2 | cut -f2)

case "$VALUE" in
	"0" ) NEW_VALUE=1 ;;
	"1" ) NEW_VALUE=0 ;;
	 *  ) echo "Error: VALUE = |$VALUE|" 1>&2 ; exit 1 ;;
esac
xinput set-prop $TOUCHPAD_ID "$NAME" $NEW_VALUE

