#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) {big|small}" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi

if [ "$1" = "big" ]; then
	DEVICE="ATSAM4E16E"
	INTERFACE="JTAG"
else
	if [ "$1" = "small" ] || [ "$1" = "little" ]; then
		DEVICE="ATSAMV71Q21"
		INTERFACE="SWD"
	else
		usage
	fi
fi

set -x
~/other/programs/JLink_Linux_V512a_x86_64/JLinkGDBServer -device "$DEVICE" -endian little -if "$INTERFACE" -speed auto

