#!/bin/bash

usage()
{
	echo -e "Usage: $(basename $0) CARD_PREFIX" 1>&2
	exit 1
}

if [ $# -ne 1 ] ; then
	usage
fi
CARD_PREFIX="$1"

# Get full sink name:
RET=1
while [ $RET -ne 0 ]; do
	RESULT=$(LANGUAGE=en pacmd list card | grep "name: <$CARD_PREFIX\.")
	RET=$?
done
CARD_NAME="$(echo "$RESULT" | cut -d'<' -f2 | cut -d'>' -f1)"

# Print full card name:
echo "$CARD_NAME"

