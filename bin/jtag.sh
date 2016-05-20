#!/bin/bash

set -e
source ~/os_settings/other_files/jtag_device_variables.sh
set -x
~/other/programs/JLink_Linux_V512a_x86_64/JLinkGDBServer -device "$DEVICE" -if "$INTERFACE" -speed auto -endian little

