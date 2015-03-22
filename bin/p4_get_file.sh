#!/usr/bin/env bash

USAGE="Usage:\n\
$(basename $0) \"//depot/path/to/file\"\n\
The file will be downloaded to current directory."

if [ $# -ne 1 ]; then
	echo -e $USAGE 1>&2
	exit 1
fi

DEPOT_FILE="$1"
FILE_NAME="$(echo $DEPOT_FILE | rev | cut -d'/' -f 1 | rev)"
echo -e "\nExecuting: p4 print -o \"$FILE_NAME\" \"$DEPOT_FILE\" ..."
OUTPUT=$(p4 print -o "$FILE_NAME" "$DEPOT_FILE" 2>&1)
EXIT_CODE=$?
if [ $EXIT_CODE -ne 0 ]; then
	echo -e "\nThe following error occured:\n$OUTPUT\n\nExit code: $EXIT_CODE" 1>&2
	exit $EXIT_CODE
fi
if echo $OUTPUT | grep -q 'no such file(s)' ; then
	echo -e "\nThe following error occured:\n$OUTPUT" 1>&2
	exit 1
fi
set -e
# Add write permission:
chmod u+w "$FILE_NAME"
# Copy file name to clipboard:
# echo "\"$FILE_NAME\"" | clipboard.sh
# echo -e "\nDone. See: \"$FILE_NAME\" (copied to your clipboard)."
echo -e "\nDone. See: \"$FILE_NAME\"."

