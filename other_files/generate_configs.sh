#!/bin/bash

IN_FILES=(
.config_xdg/i3/config
.Xmodmap
)
OUT_FILES=("${IN_FILES[@]/#/~/}")
IN_S_FILES=("${IN_FILES[@]/#/${DOTRC_S}/home_settings/}")
IN_FILES=("${IN_FILES[@]/#/${DOTRC}/home_settings/}")
# echo ${IN_FILES[@]}

$DOTRC/other_files/virtual_box.sh
VIRTUAL=$?

if [ $VIRTUAL -eq 1 ] ; then
    # Native [begin ; '# SED virtual begin'], ['# SED virtual end' ; end]
    set -e
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
    set -e
    for ((i=0; i<${#IN_FILES[@]}; ++i)) ; do
        in_file=${IN_FILES[$i]}
        out_file=${OUT_FILES[$i]}

        NATIVE_BEGIN=$(grep -n '# SED native begin$' "$in_file" | cut -d':' -f1)
        NATIVE_END=$(grep -n '# SED native end$' "$in_file" | cut -d':' -f1)

        head "$in_file" -n $NATIVE_BEGIN > "$out_file"
        tail "$in_file" -n +$NATIVE_END >> "$out_file"
    done
fi

set -e
for ((i=0; i<${#IN_S_FILES[@]}; ++i)) ; do
    in_s_file=${IN_S_FILES[$i]}
    out_file=${OUT_FILES[$i]}
    if [ -e "$in_s_file" ]; then
        cat "$in_s_file" >> "$out_file"
    fi
done
