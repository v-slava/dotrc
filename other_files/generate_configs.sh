#!/bin/bash

case $# in
    0) FOR_ALL_FILES=true ;;
    1) FOR_FILE="$1" ;;
    *) echo "Usage: $(basename $0) [FILE]" 1>&2 ; exit 1 ;;
esac

CONCAT_CONFIGS=$DOTRC/other_files/concat_config.sh

set -e

if [ "$FOR_ALL_FILES" = "true" ]; then
    if [ -d $DOTRC_S/home_settings ]; then
        cd $DOTRC_S/home_settings
        FILES_LIST=$(find -type f | grep -v '\.bashrc')
        for FILE in $FILES_LIST ; do
            $CONCAT_CONFIGS "$FILE"
        done
    fi
else
     case "$FOR_FILE" in
         ".bashrc") ;;
         *) $CONCAT_CONFIGS "$FOR_FILE"
     esac
fi

if [ "$FOR_ALL_FILES" = "true" ] || \
   [ "$FOR_FILE" = ".config_xdg/i3/config" ]; then
    $DOTRC/other_files/xrandr.sh update_i3_config
fi

IN_FILES=(
    ".Xmodmap"
)

if [ "$FOR_ALL_FILES" != "true" ]; then
    array_contains_element () {
        local element="$1"
        shift
        for i in "$@" ; do
            if [ "$i" == "$element" ]; then
                return 0
            fi
        done
        return 1
    }
    if array_contains_element "$FOR_FILE" "${IN_FILES[@]}" ; then
        IN_FILES=("$FOR_FILE")
    else
        exit 0 # we are done here
    fi
fi

OUT_FILES=("${IN_FILES[@]/#/~/}")
IN_S_FILES=("${IN_FILES[@]/#/${DOTRC_S}/home_settings/}")
IN_FILES=("${IN_FILES[@]/#/${DOTRC}/home_settings/}")
# echo ${IN_FILES[@]}

set +e
$DOTRC/other_files/virtual_box.sh
VIRTUAL=$?
set -e

if [ $VIRTUAL -eq 1 ] ; then
    # Native [begin ; '# SED virtual begin'], ['# SED virtual end' ; end]
    for ((i=0; i<${#IN_FILES[@]}; ++i)) ; do
        in_file=${IN_FILES[$i]}
        out_file=${OUT_FILES[$i]}

        VIRTUAL_BEGIN=$(grep -n '# SED virtual begin$' "$in_file" | cut -d':' -f1)
        VIRTUAL_END=$(grep -n '# SED virtual end$' "$in_file" | cut -d':' -f1)

        head "$in_file" -n $VIRTUAL_BEGIN > "$out_file"
        tail "$in_file" -n +$VIRTUAL_END >> "$out_file"
    done
else
    # VirtualBox [begin ; '# SED native begin'], ['# SED native end' ; end]
    for ((i=0; i<${#IN_FILES[@]}; ++i)) ; do
        in_file=${IN_FILES[$i]}
        out_file=${OUT_FILES[$i]}

        NATIVE_BEGIN=$(grep -n '# SED native begin$' "$in_file" | cut -d':' -f1)
        NATIVE_END=$(grep -n '# SED native end$' "$in_file" | cut -d':' -f1)

        head "$in_file" -n $NATIVE_BEGIN > "$out_file"
        tail "$in_file" -n +$NATIVE_END >> "$out_file"
    done
fi

for ((i=0; i<${#IN_S_FILES[@]}; ++i)) ; do
    in_s_file=${IN_S_FILES[$i]}
    out_file=${OUT_FILES[$i]}
    if [ -e "$in_s_file" ]; then
        cat "$in_s_file" >> "$out_file"
    fi
done
