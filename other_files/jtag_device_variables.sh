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

case $1 in
	big)
	DEVICE=ATSAM4E16E
	INTERFACE=JTAG
	SRAM_START_ADDRESS="20000000"
	SRAM_END_ADDRESS="20400000"
	FLASH_START_ADDRESS="00400000"
	FLASH_END_ADDRESS="00500000"
	;;
	small|little)
		DEVICE=ATSAMV71Q21
		INTERFACE=SWD
		SRAM_START_ADDRESS="20400000"
		SRAM_END_ADDRESS="20C00000"
		FLASH_START_ADDRESS="00400000"
		FLASH_END_ADDRESS="00600000"
	;;
	*)
		usage
	;;
esac
# shift
SRAM_SIZE=$(echo "ibase=16; $SRAM_END_ADDRESS - $SRAM_START_ADDRESS" | bc)
FLASH_SIZE=$(echo "ibase=16; $FLASH_END_ADDRESS - $FLASH_START_ADDRESS" | bc)
# FLASH_SIZE=$(($((16#$FLASH_END_ADDRESS)) - $((16#$FLASH_START_ADDRESS))))

