#!/usr/bin/env bash

USAGE="USAGE: $(basename $0) {PROJECT_ROOT_DIRECTORY|-l FILES_LIST}"
set -e
usage()
{
	echo -e "$USAGE" 1>&2
	exit 1
}

CTAGS="ctags-exuberant --c++-kinds=+p --fields=+iaS --extra=+q"
CSCOPE="cscope -b -k"

if [ $# -eq 2 ]; then
	if [ "$1" != "-l" ]; then
		usage
	fi
	FILES_LIST="$2"
	if [ ! -f "$FILES_LIST" ]; then
		echo -e "Error: FILES_LIST=\"$FILES_LIST\" - file not found" 1>&2
		exit 1
	fi

	# Index via ctags:
	$CTAGS -L $FILES_LIST

	# Index via cscope:
	$CSCOPE -i$FILES_LIST
	exit
fi


if [ $# -ne 1 ]; then
	usage
fi

ROOT_DIR="$1"
if [ ! -d "$ROOT_DIR" ]; then
	echo -e "Error: no such directory: $ROOT_DIR.\n$USAGE" 1>&2
	exit 1
fi

# Need to use "readlink -f" to canonicalize path?
CURRENT_DIR="$PWD"
cd "$ROOT_DIR"
if [ -d "./arch" ]; then
	# Assume this is linux kernel. Generate list of files to be indexed:
	FILES_LIST=/tmp/src_files_to_index
	find .									\
		-path "./tmp/*" -prune -o			\
		-path "./Documentation/*" -prune -o	\
		-path "./scripts/*" -prune -o		\
		-path "./arch/*" -prune -o			\
		-name "*.[chxsS]" -print > $FILES_LIST
	find ./arch/arm							\
		-name "*.[chxsS]" -print >> $FILES_LIST

	# Index via ctags:
	$CTAGS -L $FILES_LIST

	# Index via cscope:
	$CSCOPE -i$FILES_LIST

	rm -f $FILES_LIST
else
	$CTAGS -R .
	$CSCOPE -R
fi
# Create cross-reference tag for CCTree:
# ccglue -S cscope.out -o cctree.out
cd "$CURRENT_DIR"

