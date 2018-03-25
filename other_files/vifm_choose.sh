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
ORIG_WORKSPACE=$(~/os_settings/other_files/i3_get_focused_workspace.sh)
i3-msg "workspace 0" 1>/dev/null
case $WHAT in
    -d) x-terminal-emulator -title 'Choose directory by pressing "q":' -e vifm --choose-dir $FILE ;;
    -fs) x-terminal-emulator -title 'Choose single file by pressing "l":' -e vifm --choose-files $FILE ;;
    -fm) x-terminal-emulator -title 'Choose multiple files by selecting them and pressing "l" afterwards:' -e vifm --choose-files $FILE ;;
    *) usage ;;
esac
i3-msg "workspace $ORIG_WORKSPACE" 1>/dev/null
if [ "$1" = "-n" ]; then
    cat $FILE | tr -d '\n'
else
    cat $FILE
fi
rm -f $FILE
