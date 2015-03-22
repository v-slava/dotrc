#!/bin/bash

# Returns 0 (success) if bluetooth headset is connected and 1 (fail) otherwise

hcitool con | grep -q $BT_MAC

