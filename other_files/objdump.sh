#!/bin/bash

# This is an objdump wrapper script. It determines target file type and runs
# proper objdump executable.

# last command line argument:
FILE="${@: -1}"

FILE_INFO="$(file "$(realpath "$FILE")")"

case "$FILE_INFO" in
    *ARM*) CROSS_COMPILE=arm-linux-gnueabihf- ;;
    *PowerPC*) CROSS_COMPILE=powerpc-linux-gnu- ;;
    *) CROSS_COMPILE= ;;
esac
${CROSS_COMPILE}objdump $@
