#!/bin/bash

# Toggles bluetooth connection status:
# If was connected    -> disconnecting
# If was disconnected -> connecting

if bluetooth_connected.sh ; then
	echo -e "\nDisconnecting ...\n"
	echo "disconnect $BT_MAC" | bluetoothctl
else
	echo -e "\nConnecting ...\n"
	echo "connect $BT_MAC" | bluetoothctl
fi

