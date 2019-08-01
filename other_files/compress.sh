#!/usr/bin/env bash

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = 'silent wa | RunShellCmd cd ~/downloads && $DOTRC/other_files/compress.sh -e extracted_u-boot-cdbeae407845a936f5b2b90e7683c5544f93f34b.tar.bz2'
# EVAL REGION ENDS HERE.

usage()
{
	echo "Usage: $(basename $0) [-e] FILE" 1>&2
	exit 1
}

if [ $# -lt 1 ]; then
	usage
fi

EXTRACTED="false"
if [ "$1" = "-e" ]; then
	EXTRACTED="true"
	shift
fi

FILE="$1"
if [ ! -f "$FILE" ] && [ ! -d "$FILE" ]; then
	echo "Error: file/folder not found: \"$FILE\"." 1>&2
	usage
fi
echo "Compressing \"$FILE\"..."
if [ "$EXTRACTED" != "true" ]; then
	set -e
	tar cfz "${FILE}.tar.gz" "$FILE"
	exit 0
fi

if ! echo "$FILE" | grep -q '^extracted_' ; then
	usage
fi

# cut "extracted_" prefix from $FILE:
OUT_FILE="$(echo "$FILE" | cut -d'_' -f2-)"

if [ -f "$OUT_FILE" ]; then
	echo "Error: will not overwrite existing file: $OUT_FILE" 1>&2
	exit 1
fi

case "$OUT_FILE" in
	*.tar) tar cf ${OUT_FILE} -C ${FILE} . ;;
	*.tgz | *.tar.gz) tar czf ${OUT_FILE} -C ${FILE} . ;;
	*.tar.bz2 | *.tbz2) tar cjf ${OUT_FILE} -C ${FILE} . ;;
	*.tar.xz | *.txz) tar cJf ${OUT_FILE} -C ${FILE} . ;;
	*)
		echo "Error: \"$FILE\" - unknown archive format." 1>&2
		exit 1
		;;
esac
