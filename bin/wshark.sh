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

TCPDUMP=$(which tcpdump)
TCPDUMP_PERMISSIONS=$(ls -l $TCPDUMP | cut -d' ' -f1)
if ! echo $TCPDUMP_PERMISSIONS | grep -q s ; then
    echo "$TCPDUMP doesn't have SUID permission bit set.\n\
To fix, please execute the following command:\n\
su -c \"chmod u+s $TCPDUMP\"" 1>&2
    exit 1
fi

FIFO=/tmp/wireshark_tcpdump_fifo

rm -f $FIFO
mkfifo $FIFO

tcpdump -i "$1" -w - -U > $FIFO &
cat $FIFO | wireshark -k -i -
killall tcpdump
rm -f $FIFO

