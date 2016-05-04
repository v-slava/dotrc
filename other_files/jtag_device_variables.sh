# This script should be sourced.

if ! type usage 1>/dev/null 2>&1 ; then
	usage()
	{
		echo "Usage: $(basename $0) {big|small} [SOME_OTHER_ARGUMENTS]" 1>&2
		exit 1
	}
fi

if [ $# -lt 1 ]; then
	usage
fi

if [ "$1" = "big" ]; then
	DEVICE=ATSAM4E16E
	INTERFACE=JTAG
	SRAM_START_ADDRESS=0x20000000
	SRAM_END_ADDRESS=0x20400000
	FLASH_START_ADDRESS=0x00400000
	FLASH_END_ADDRESS=0x00800000
else
	if [ "$1" = "small" ] || [ "$1" = "little" ]; then
		DEVICE=ATSAMV71Q21
		INTERFACE=SWD
		SRAM_START_ADDRESS=0x20400000
		SRAM_END_ADDRESS=0x20C00000
		FLASH_START_ADDRESS=0x00400000
		FLASH_END_ADDRESS=0x00800000
	else
		usage
	fi
fi
# shift
SRAM_SIZE=$(echo "ibase=16; $(echo $SRAM_END_ADDRESS | cut -c 3-) - $(echo $SRAM_START_ADDRESS | cut -c 3-)" | bc)
FLASH_SIZE=$(echo "ibase=16; $(echo $FLASH_END_ADDRESS | cut -c 3-) - $(echo $FLASH_START_ADDRESS | cut -c 3-)" | bc)

