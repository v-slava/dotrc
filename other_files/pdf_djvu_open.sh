#!/usr/bin/env bash

PASSWORD_FILE_PREFIX=/media/files/passwords/pdf_djvu_password

if [ $# -ne 1 ]; then
	echo "Usage: $(basename $0) FILE.{pdf|djvu}" 1>&2
	exit 1
fi
FILE="$1"

CMD="zathura --fork"
if [ "$(echo "$FILE" | cut -c -5)" = "pass_" ]; then
	PASSWORD_FILE_SUFFIX=$(echo "$FILE" | cut -c 6- | cut -d'_' -f1)
	PASSWORD_FILE=$(echo "${PASSWORD_FILE_PREFIX}_${PASSWORD_FILE_SUFFIX}")
	if [ -f $PASSWORD_FILE ]; then
		PASSWORD=$(head -n 1 $PASSWORD_FILE)
		CMD="$CMD -w '$PASSWORD'"
	fi
fi
CMD="$CMD '$FILE' 2>/dev/null 1>&2"
echo $CMD | bash

