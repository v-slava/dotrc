source ~/os_settings/other_files/ide_common.sh
if [ $# -lt 1 ]; then
	echo $USAGE 1>&2
	exit 1
fi
IN="$1"
OUT="${IDE_DIR}/${IN}.out"
