#!/bin/bash

usage()
{
	echo -e "Usage: $(basename $0) INTERFACE

List of available interfaces:\n" 1>&2
	tcpdump -D 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi
tcpdump -i "$1" -w - -U | wireshark -k -i -

