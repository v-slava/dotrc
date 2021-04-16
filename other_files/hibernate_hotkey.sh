#!/bin/bash
set -x

POSITIONAL=()
while [ $# -gt 0 ] ; do
    case "$1" in
        "--sleep") shift; SLEEP="$1" ; shift ;;
        *) POSITIONAL+=("$1") ; shift ;;
    esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

set -e

if ! $DOTRC/other_files/virtual_box.sh ; then
    $DOTRC/other_files/lock_screen.sh &
fi

CMD="$DOTRC/other_files/update_system.sh ; exec sudo poweroff"
# sudo systemctl hibernate

if [ -n "$SLEEP" ]; then
    CMD="$DOTRC/other_files/sleep.sh $SLEEP 'shutdown / hibernate' && $CMD"
fi

$DOTRC/other_files/open_terminal.sh --title "hibernate script" bash -c "$CMD"
