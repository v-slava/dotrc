#!/bin/bash

# This is an objdump wrapper script. It determines target file type and runs
# proper objdump executable (either objdump or ${CROSS_COMPILE}objdump)

# last command line argument:
FILE="${@: -1}"

if file "$FILE" | grep -q "ARM" ; then
	OBJDUMP=${CROSS_COMPILE}objdump
else
	OBJDUMP=objdump
fi
$OBJDUMP $@
