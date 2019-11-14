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

if [ ! -d "$OUT" ]; then
    mkdir "$OUT"
fi

# Check whether all original files exist:
for i in $(seq 0 $LAST_INDEX); do
    if [ ! -f "${ORIGINAL_FILE[$i]}" ]; then
        echo "Error: \"${ORIGINAL_FILE[$i]}\" not found" 1>&2
        exit 1
    fi
done

for i in $(seq 0 $LAST_INDEX); do

    # Save original file if needed:
    if [ ! -f "${SAVED_FILE[$i]}" ]; then
        mv "${ORIGINAL_FILE[$i]}" "${SAVED_FILE[$i]}"
    fi

    echo -e -n "#!/bin/bash

CURRENT_FILE=\"${ORIGINAL_FILE[$i]}\"
CURRENT_FILE_NAME=\"$(basename "${ORIGINAL_FILE[$i]}")\"
ORIGINAL_FILE=\"${SAVED_FILE[$i]}\"
OUT=\"$OUT\"

# Custom actions:
" > "${ORIGINAL_FILE[$i]}"
    chmod +x "${ORIGINAL_FILE[$i]}"

    if [ -f "$CUSTOM_ACTIONS_SH" ]; then
        cat "$CUSTOM_ACTIONS_SH" >> "${ORIGINAL_FILE[$i]}"
    fi

    # Copy fake binary to OUT folder:
    cp "${ORIGINAL_FILE[$i]}" "$OUT/"
done
