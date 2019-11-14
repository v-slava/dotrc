#!/bin/bash

set -e

COMPILER_SH="$(echo "$PWD/$(dirname $0)/compiler_settings/compiler.sh")"
CUSTOM_ACTIONS_SH="$(echo "$PWD/$(dirname $0)/compiler_settings/custom_actions.sh")"
OUT="$(echo "$PWD/$(dirname $0)/out")"

if [ ! -f "$COMPILER_SH" ]; then
    echo "Error: \"$COMPILER_SH\" not found" 1>&2
    exit 1
fi
. "$COMPILER_SH"

# Check whether original files exist:
for i in $(seq 0 $LAST_INDEX); do
    if [ ! -f "${SAVED_FILE[$i]}" ]; then
        echo "Error: \"${SAVED_FILE[$i]}\": file not found" 1>&2
        exit 1
    fi
done

# Restore original files:
for i in $(seq 0 $LAST_INDEX); do
    mv "${SAVED_FILE[$i]}" "${ORIGINAL_FILE[$i]}"
done
