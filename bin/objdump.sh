#!/bin/bash

# This is an objdump wrapper script. It determines target file type and runs
# proper objdump executable.

# last command line argument:
FILE="${@: -1}"

FILE_INFO="$(file "$(realpath "$FILE")")"

case "$FILE_INFO" in
    *ELF*) ;;
    *) echo "Can't disassemble: not an ELF file!" 1>&2 ; exit 1 ;;
esac

# case "$FILE_INFO" in
#     *32-bit*ARM*) CROSS_COMPILE=arm-linux-gnueabihf- ;;
#     *64-bit*ARM*) CROSS_COMPILE=aarch64-linux-gnu- ;;
#     *PowerPC*) CROSS_COMPILE=powerpc-linux-gnu- ;;
#     *) CROSS_COMPILE= ;;
# esac

CROSS_COMPILE=llvm-

${CROSS_COMPILE}objdump $@
