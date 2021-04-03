#!/bin/bash

set -e

usage()
{
    cat << EOF 1>&2
Usage: $(basename $0) WHAT [-n]

WHAT can be one of the following values:
    -d
        choose directory.

    -fs
        choose single file.

    -fm
        choose single or many files.

-n
    no newline at the end of output (not applicable for -fm).
EOF
    exit 1
}

if [ $# -lt 1 ]; then
    usage
fi
WHAT="$1"
shift
case $WHAT in
    -d | -fs | -fm) ;;
    *) usage ;;
esac
if [ "$1" = "-n" ]; then
    if [ "$WHAT" = "-fm" ]; then
        usage
    fi
fi

FILE=/tmp/vifm_selected
rm -f $FILE
ORIG_WORKSPACE=$($DOTRC/other_files/i3_get_focused_workspace.py)

# For i3wm:
# source $DOTRC/other_files/i3_msg.sh
# i3_msg "workspace 0"
# For sway:
swaymsg "workspace 0"

case $WHAT in
    -d) $DOTRC/other_files/open_terminal.sh --title \
        'Choose directory by pressing "q":' vifm --choose-dir $FILE \
        2>/dev/null ;;
    -fs) $DOTRC/other_files/open_terminal.sh --title \
        'Choose single file by pressing "l":' vifm --choose-files $FILE \
        2>/dev/null ;;
    -fm) $DOTRC/other_files/open_terminal.sh --title \
        'Choose multiple files by selecting them and pressing "l" afterwards:' \
        vifm --choose-files $FILE 2>/dev/null ;;
    *) usage ;;
esac

# For i3wm:
# i3_msg "workspace $ORIG_WORKSPACE"
# For sway:
swaymsg "workspace $ORIG_WORKSPACE"

if [ "$1" = "-n" ]; then
    cat $FILE | tr -d '\n'
else
    cat $FILE
fi
rm -f $FILE
