#!/bin/bash

DIR="$HOME/my/screenshots"
FILE="$DIR/$(date '+%Y_%m_%d__%H_%M_%S_%N')_scrot.png"

EDIT_IMAGE=false
PRINT_FILE_PATH=false

for arg do
    shift
    case "$arg" in
        "--my-print-file-path") PRINT_FILE_PATH=true ;;
        "--my-edit-image") EDIT_IMAGE=true ;;
        *) set -- "$@" "$arg" ;;
    esac
done

set -e

mkdir -p "$DIR"
scrot "$@" "$FILE"
if [ "$EDIT_IMAGE" = "true" ]; then
    kolourpaint "$FILE"
fi
if [ "$PRINT_FILE_PATH" = "true" ]; then
    echo "$FILE"
fi
