#!/bin/bash

if [ $# -ne 1 ] ; then
	echo "Usage: $(basename $0) GREP_PATTERN" 1>&2
	exit 1
fi
PATTERN="$1"

# grep "$PATTERN"

while read line ; do
	column=$(echo "$line" | cut -d':' -f3)
	line_part=$(echo "$line" | cut -d':' -f4- | cut -c $column-)
	echo "$line_part" | grep -q "$PATTERN"
	if [ $? -eq 0 ]; then
		echo $line
	fi
done

