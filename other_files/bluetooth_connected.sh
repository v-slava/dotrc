#!/bin/bash

# Returns 0 (success) if bluetooth headset is connected and 1 (fail) otherwise

source ~/.config_xdg/BT_MAC.sh

# hcitool con | grep -q $BT_MAC

echo "info $BT_MAC" | bluetoothctl | grep 'Connected: ' | cut -d' ' -f2 | grep -q 'yes'
