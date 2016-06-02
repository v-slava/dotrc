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

set -e
# Get full sink name:
CARD_NAME="$(LANGUAGE=en pacmd list cards | grep "name: <$CARD_PREFIX\." | cut -d'<' -f2 | cut -d'>' -f1)"
# Print full card name:
echo "$CARD_NAME"

