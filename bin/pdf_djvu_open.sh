#!/usr/bin/env bash

PASSWORD_FILE=/media/files/passwords/pdf_djvu_password

if [ $# -ne 1 ]; then
	echo "Usage: $(basename $0) FILE.{pdf|djvu}" 1>&2
	exit 1
fi
FILE="$1"

CMD="zathura"
if [ -f $PASSWORD_FILE ]; then
	PASSWORD=$(head -n 1 $PASSWORD_FILE)
	CMD="$CMD -w '$PASSWORD'"
fi
CMD="$CMD '$FILE' 2>/dev/null 1>&2"
echo $CMD | bash

