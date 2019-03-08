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

source $DOTRC/other_files/config_file.sh

case "$1" in
    "dotrc")
        EDIT_FILE="$DOTRC_FILE"
        ;;
    "dotrc_s")
        EDIT_FILE="$DOTRC_S_FILE"
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
    ORIG_WORKSPACE=$($DOTRC/other_files/i3_get_focused_workspace.sh)
    i3-msg "workspace 0" 1>/dev/null
fi

e --wait "$EDIT_FILE"

if [ "$SWITCH_WORKSPACE" = "true" ]; then
    i3-msg "workspace $ORIG_WORKSPACE" 1>/dev/null
fi

$DOTRC/other_files/update_config.sh "$FILE"
$DOTRC/other_files/generate_configs.sh
