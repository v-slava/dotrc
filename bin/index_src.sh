#!/usr/bin/env bash

USAGE="USAGE: $(basename $0) [FILES_LIST]"
set -e
usage()
{
    echo -e "$USAGE" 1>&2
    exit 1
}
set -x

CTAGS="ctags-exuberant --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=no -e"
CSCOPE="cscope -b -k"

if [ $# -eq 1 ]; then
    FILES_LIST="$1"
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

if [ $# -ne 0 ]; then
    usage
fi

if [ -d "./arch" ]; then
    # Assume this is linux kernel. Generate list of files to be indexed:
    FILES_LIST=/tmp/src_files_to_index
    find .                                     \
        -path "./tmp/*" -prune -o              \
        -path "./Documentation/*" -prune -o    \
        -path "./scripts/*" -prune -o          \
        -path "./arch/*" -prune -o             \
        -type f -name "*.[chxsS]" -print > $FILES_LIST
    find ./arch/arm                            \
        -type f -name "*.[chxsS]" -print >> $FILES_LIST

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
