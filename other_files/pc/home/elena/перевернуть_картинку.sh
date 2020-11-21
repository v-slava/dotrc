#!/bin/bash

set -e
IN="$1"
OUT="$(echo "$IN" | cut -d'.' -f-1)_перевернутое.$(echo "$IN" | cut -d'.' -f2-)"
OUT_CUT="$(echo "$OUT" | rev | cut -d'/' -f1 | rev)"
convert -rotate 90 "$IN" "$OUT"
