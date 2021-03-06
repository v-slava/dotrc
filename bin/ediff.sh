#!/usr/bin/env bash

# export USE_VIM=true

usage()
{
    echo -e "Usage: $(basename $0) LEFT RIGHT\n\
Where both LEFT and RIGHT are both either folders or files.\n" 1>&2
    exit 1
}

if [ $# -ne 2 ]; then
    usage
fi

LEFT="$(realpath "$1")"
RIGHT="$(realpath "$2")"
cd

EDIFF_COMMAND='e --diff'

# echo "LEFT = |$LEFT|"
# echo "RIGTH = |$RIGHT|"
# exit

if [ ! -e "$LEFT" ]; then
    usage
fi
if [ ! -e "$RIGHT" ]; then
    usage
fi

FILES_TO_DIFF=()

process_file()
{
    echo "file = $1"
    FILES_TO_DIFF+=("$1")
}

if [ ! -d "$LEFT" ]; then
    if [ -d "$RIGHT" ]; then
        usage
    fi
    # both files
    $EDIFF_COMMAND "$LEFT" "$RIGHT"
else
    if [ ! -d "$RIGHT" ]; then
        usage
    fi
    # both folders

    reg_exp="^Files /(.+) and /(.+) differ$"
    exec 6<&0 # save original stdin
    diff -qr "$LEFT" "$RIGHT" | grep -v '^Only in ' | while read line ; do
        exec 7<&0 # save stdin for reading from grep
        echo "line = $line"
        if [[ $line =~ $reg_exp ]]; then
            exec 0<&6 # restore original stdin
            $EDIFF_COMMAND "/${BASH_REMATCH[1]}" "/${BASH_REMATCH[2]}"
            exec 0<&7 # restore stdin for reading from grep
        fi
    done
    exec 6<&-
    exec 7<&-
fi
