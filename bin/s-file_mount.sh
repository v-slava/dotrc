#!/usr/bin/env bash

USAGE="\n\
USAGE: $(basename $0) RESOURCE_TO_MOUNT\n\
\n\
For all available resources see output of the following command:\n\
\n\
smbclient -U v.volkov -W SURC -L s-file.surc.kiev.ua\n\
\n\
Currently supported resources:\n\
\n\
Projects\n\
Exchange\n\
Install\n"

if [ $# -ne 1 ]; then
	echo -e "$USAGE" 1>&2
	exit 1
fi

RESOURCE_TO_MOUNT="$1"

# execute as root:
mount -t cifs -o user=v.volkov,domain=SURC //s-file.surc.kiev.ua/$RESOURCE_TO_MOUNT /media/s-file/$RESOURCE_TO_MOUNT

