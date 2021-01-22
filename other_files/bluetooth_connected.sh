#!/bin/bash

# Returns 0 (success) if bluetooth headset is connected and 1 (fail) otherwise

# hcitool con | grep -q $BT_MAC

echo "info $BT_MAC" | bluetoothctl | grep 'Connected: ' | cut -d' ' -f2 | grep -q 'yes'
