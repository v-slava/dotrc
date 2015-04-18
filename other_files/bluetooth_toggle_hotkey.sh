#!/bin/bash

# Toggles bluetooth connection status:
# If was connected    -> disconnecting
# If was disconnected -> connecting

BLUETOOTH_TOGGLE_SH=~/os_settings/other_files/bluetooth_toggle.sh

x-terminal-emulator -title "My bluetooth headset" -geometry 70x13 -e bash -c \
	"$BLUETOOTH_TOGGLE_SH ; echo ; vifm-pause"

