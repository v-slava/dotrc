#!/usr/bin/env bash

set -e

FILE="$1"
source $DOTRC/other_files/config_file.sh

cp "$DOTRC_FILE" ~/$FILE
if [ -f "$DOTRC_S_FILE" ]; then
    cat "$DOTRC_S_FILE" >> ~/$FILE
fi
