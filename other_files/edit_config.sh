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

DOTRC_PATH=$DOTRC/home_settings
DOTRC_S_PATH=$DOTRC_S/home_settings
DOTRC_FILE="$DOTRC_PATH/$FILE"
DOTRC_S_FILE="$DOTRC_S_PATH/$FILE"
if [ ! -f "$DOTRC_FILE" ]; then
    echo "File not found: $DOTRC_FILE" 1>&2
    exit 2
fi

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

e --wait "$EDIT_FILE"
cp "$DOTRC_FILE" ~/$FILE
if [ -f "$DOTRC_S_FILE" ]; then
    cat "$DOTRC_S_FILE" >> ~/$FILE
fi
