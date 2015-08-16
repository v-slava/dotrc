#!/usr/bin/env bash

# Usage: echo "contents" | $0 [-n|-o|-copy]
# -n    - remove \n from input stream
# -o    - print clipboard contents to stdout
# -copy - copy to primary selection

# not works for vim clipboard preserving in st (* register):
# CLIPBOARD_CMD="xsel -p"
# works for vim clipboard preserving in st (* register):
# CLIPBOARD_CMD="xclip -selection primary"
CLIPBOARD_CMD="xclip -selection clipboard"

case "$1" in
	("-o") $CLIPBOARD_CMD -o ;;
	("-n") cat | tr -d '\n' | $CLIPBOARD_CMD ;;
	("-copy") $CLIPBOARD_CMD -o | xclip -selection primary ;;
	(*) $CLIPBOARD_CMD -i ;;
esac

