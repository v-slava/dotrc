#!/usr/bin/env bash

# Usage: echo "contents" | $0 [-n|-o|-copy]
# -n    - remove \n from input stream
# -o    - print clipboard contents to stdout
# -copy - copy to primary selection

# does not work for vim clipboard preserving in st (* register):
# CLIPBOARD_CMD="xsel -p"
# works for vim clipboard preserving in st (* register):
# CLIPBOARD_CMD="xclip -selection primary"
#
# hangs when called from within kakoune:
# xclip -selection clipboard -i
# does not hang when called from within kakoune:
# xsel --clipboard -i

# does not copy a selection of more than 4000 bytes of selection on urxvt screen:
# xsel --clipboard -i
# copies fine:
# xclip -selection clipboard

# CLIPBOARD_CMD="xclip -selection clipboard"
CLIPBOARD_CMD="xsel --clipboard"

case "$1" in
    "-o") $CLIPBOARD_CMD -o ;;
    "-n") cat | tr -d '\n' | $CLIPBOARD_CMD -i ;;
    "--clipboard-2-primary") xsel --clipboard -o | xsel -i ;;
    "--primary-2-clipboard") xsel -o | xsel --clipboard -i ;;
    # "--clipboard-2-primary") xclip -selection clipboard -o | xclip -selection primary -i ;;
    # "--primary-2-clipboard") xclip -selection primary -o | xclip -selection clipboard -i ;;
    *) $CLIPBOARD_CMD -i ;;
esac
