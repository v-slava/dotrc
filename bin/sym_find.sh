#!/bin/bash

# Search through all *.a and *.o files in a given directory for a given symbol.
# NM_FLAGS: --demangle -g

usage()
{
	echo "Usage: $(basename $0) DIR SYMBOL [NM_FLAGS]" 1>&2
	exit 1
}

if [ $# -lt 2 ]; then
	usage
fi

DIR="$1"
SYMBOL="$2"
shift
shift
NM_FLAGS="$@"

if [ ! -d "$DIR" ]; then
	usage
fi
set -e
DIR="$(realpath "$DIR")"

echo ">>> Object files:"
objs=$(find "$DIR" -name "*.o")
for obj in $objs; do
	if nm $NM_FLAGS $obj | grep -q "$SYMBOL" ; then
		# symbol was found
		echo "> Obj: $obj"
		nm $NM_FLAGS $obj | grep "$SYMBOL"
	fi
done

TMP_DIR=/tmp/find_symbol_sh
rm -rf $TMP_DIR
mkdir $TMP_DIR
SEPARATOR="-- next obj file --"
echo ">>> Libs:"
libs=$(find "$DIR" -name "*.a")
for lib in $libs; do
	rm -f $TMP_DIR/*
	nm $NM_FLAGS $lib | sed -e "s/^$/$SEPARATOR/g" | awk -v RS="$SEPARATOR" "{ print \$0 > \"$TMP_DIR/obj\" NR }"
	rm $TMP_DIR/obj1
	txt_files=$(ls $TMP_DIR/obj*)
	for txt_file in $txt_files ; do
		if tail -n +3 "$txt_file" | grep -q "$SYMBOL" ; then
			# symbol was found
			echo -n "> Lib: $lib. Obj: "
			sed -n '2p' < "$txt_file"
			tail -n +3 "$txt_file" | grep "$SYMBOL"
		fi
	done
done

