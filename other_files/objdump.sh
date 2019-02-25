#!/bin/bash

# This is an objdump wrapper script. It determines target file type and runs
# proper objdump executable.

# last command line argument:
FILE="${@: -1}"

FILE_INFO=$(file "$FILE")

if echo $FILE_INFO | grep -q "ARM" ; then
	CROSS_COMPILE=arm-linux-gnueabihf-
elif echo $FILE_INFO | grep -q "PowerPC" ; then
	CROSS_COMPILE=powerpc-linux-gnu-
else
	CROSS_COMPILE=
fi
${CROSS_COMPILE}objdump $@
