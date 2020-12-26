#!/bin/bash

LEFT_DIR="$1"
shift
RIGHT_DIR="$1"
shift

echo "L = $LEFT_DIR"
echo "R = $RIGHT_DIR"

IS_LEFT=true
NUM_LEFT=0
ITEMS=()

starts_with()
{
    case $2 in
        "$1"*) true ;;
        *) false ;;
    esac
}

for ARG in "$@" ; do
    if [ -n "$IS_LEFT" ]; then
        if starts_with "$RIGHT_DIR/" "$ARG" ; then
            IS_LEFT= # false
            if [ $NUM_LEFT -gt 1 ]; then
                break # actual items list was on left side
            fi
            ITEMS=() # actual items list was on right side
        fi
    fi
    if [ -n "$IS_LEFT" ]; then
        NUM_LEFT=$(($NUM_LEFT + 1))
        ITEMS+=("$ARG")
    else
        ITEMS+=("$(basename "$ARG")")
    fi
done

for ITEM in "${ITEMS[@]}" ; do
    diff.sh --no-less "$LEFT_DIR/$ITEM" "$RIGHT_DIR/$ITEM"
done | less -iNR
