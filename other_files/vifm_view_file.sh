#!/bin/bash

set -e

ARG="$1"
DEST="$(realpath "$ARG")"
NUM_TERMINAL_LINES=$(tput lines)
MIME="$(file -b --mime-type "$DEST")"

case "$MIME" in
    inode/directory) tree -a "$DEST" ;;
    message/rfc822) echo email ;;
    text/*) head -n $NUM_TERMINAL_LINES "$DEST" ;;
    *) echo "Can't view: unknown file type." ;;
esac
