#!/bin/bash

set -e

usage()
{
	echo "Usage: $(basename $0) {big|small} FILE_TO_FLASH" 1>&2
	exit 1
}
source ~/os_settings/other_files/jtag_device_variables.sh

FILE_TO_FLASH="$2"
if [ ! -f "$FILE_TO_FLASH" ]; then
	usage
fi
FILE_SIZE=$(du -b "$FILE_TO_FLASH" | cut -f1)
if [ $FILE_SIZE -gt $FLASH_SIZE ]; then
	echo "Error: file is too big (maximum size is $FLASH_SIZE)." 1>&2
	exit 2
fi

JLINK=~/other/programs/JLink_Linux_V610a_x86_64/JLinkExe
JLINK_SCRIPT=/tmp/flash_board_sh.jlink
cat << EOF > $JLINK_SCRIPT
r
loadbin $FILE_TO_FLASH 0x$FLASH_START_ADDRESS
r
exit
EOF
# h
# loadbin $FILE_TO_FLASH 0x$FLASH_START_ADDRESS
# SetPC 0x$PC
# g
# h = halt, g = go, r = Reset target (RESET), ? = help


set -x
cat $JLINK_SCRIPT
$JLINK -device $DEVICE -if $INTERFACE -speed auto -JTAGConf -1,-1 -CommandFile $JLINK_SCRIPT

