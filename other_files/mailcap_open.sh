#!/bin/bash

set -e

PATTERN=FILE

usage()
{
    echo -e "Usage: $(basename $0) [--in-background] IN EXTENSION CMD\n\n\
Use $PATTERN as file argument placeholder in CMD" 1>&2
    exit 1
}

OPEN_IN_BACKGROUND=false
if [ "$1" = "--in-background" ]; then
    OPEN_IN_BACKGROUND=true
    shift
fi

if [ -z "$1" ]; then
    usage
fi
IN="$1"
shift

if [ -z "$1" ]; then
    usage
fi
EXTENSION="$1"
shift
OUT="/tmp/email.$EXTENSION"
PATTERN_FOUND=false

for arg do
    shift
    case "$arg" in
        *$PATTERN*)
            PATTERN_FOUND=true
            substituted="$(echo "$arg" | sed -e "s|$PATTERN|$OUT|g")"
            set -- "$@" "$substituted"
            ;;
        *)
            set -- "$@" "$arg"
            ;;
    esac
done

if [ "$PATTERN_FOUND" != "true" ]; then
    echo "Can't find pattern $PATTERN in command line arguments left: $@" 1>&2
    usage
fi

mv "$IN" "$OUT"
chmod u+w "$OUT"
if [ "$OPEN_IN_BACKGROUND" = "true" ]; then
    "$@" &
else
    "$@"
fi
