#!/usr/bin/env bash

USAGE="Usage: $(basename $0) {dotrc|dotrc_s} path/to/file\n\
Where path/to/file is relative to home folder."

set -e

usage()
{
    echo -e "$USAGE" 1>&2
    exit 1
}

if [ $# -ne 2 ]; then
    usage
fi

FILE="$2"

case "$1" in
    "dotrc")
        EDIT_FILE="$DOTRC/home_settings/$FILE"
        ;;
    "dotrc_s")
        EDIT_FILE="$DOTRC_S/home_settings/$FILE"
        ;;
    *)
        usage
        ;;
esac

EDIT_DIR=$(dirname "$EDIT_FILE")
if [ ! -d "$EDIT_DIR" ]; then
    mkdir -p "$EDIT_DIR"
fi

EDITOR=$(e --get-editor)

if [ "$EDITOR" = "emacs" ]; then
    SWITCH_WORKSPACE=true
fi

if [ "$SWITCH_WORKSPACE" = "true" ]; then
    ORIG_WORKSPACE=$($DOTRC/other_files/i3_get_focused_workspace.py)
    swaymsg "workspace 0"
fi

e --wait "$EDIT_FILE"

if [ "$SWITCH_WORKSPACE" = "true" ]; then
    swaymsg "workspace $ORIG_WORKSPACE"
fi

source $DOTRC/other_files/config_file.sh
config_generate -h "$FILE"
