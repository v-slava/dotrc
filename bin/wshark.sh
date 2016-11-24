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

set -e

FIFO=/tmp/wireshark_tcpdump_fifo

rm -f $FIFO
mkfifo $FIFO

tcpdump -i "$1" -w - -U > $FIFO &
cat $FIFO | wireshark -k -i -
killall tcpdump
rm -f $FIFO

