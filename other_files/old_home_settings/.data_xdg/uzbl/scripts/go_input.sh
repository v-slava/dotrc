#!/bin/sh

case "$( echo "script @scripts_dir/go_input.js" | socat - "unix-connect:$UZBL_SOCKET" )" in
    *XXXFORM_ACTIVEXXX*)
        echo -e "event KEYCMD_CLEAR\nset mode = insert" > "$UZBL_FIFO"
        ;;
esac
