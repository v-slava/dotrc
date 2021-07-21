#!/bin/bash

# WARNING: do not forget "export WAYLAND_DISPLAY=wayland-0" in all ssh sessions!

usage()
{
    cat << EOF 1>&2
Usage:
first in shell #1:      $(basename $0) user@host
afterwards in shell #2: $(basename $0) localhost
EOF
    exit 1
}

PORT=53539

REMOTE_CLIPBOARD=/tmp/remote_clipboard
LOCAL_CLIPBOARD=/tmp/local_clipboard
TX_FORBIDDEN=/tmp/clipboard_tx_forbidden
NC_ARG=

set -e

case "$1" in
    "") usage ;;
    localhost)
        # Check that all tools are installed on client side:
        command -v wl-copy 1>/dev/null
        command -v wl-paste 1>/dev/null
        command -v nc 1>/dev/null
        cat << EOF
Now your clipboard is shared!
Don't forget

export WAYLAND_DISPLAY=wayland-0

in all ssh sessions!
EOF
        ;;
    --server) NC_ARG=-l ;;
    --from-local)
        if [ -f $TX_FORBIDDEN ]; then
            rm -f $TX_FORBIDDEN
        else
            cat > $LOCAL_CLIPBOARD
            NUM_BYTES=$(du -b $LOCAL_CLIPBOARD | cut -f1)
            if [ $NUM_BYTES -gt 33554432 ]; then # 32 Mbytes
                exit # skip copying big amount of data over network
            fi
            ASCII_HEX_NUM_BYTES=$(echo $NUM_BYTES | xargs printf "%08x")
            # Write file size (8 ASCII hex digits):
            echo -n $ASCII_HEX_NUM_BYTES
            cat $LOCAL_CLIPBOARD
        fi
        exit
        ;;
    --from-remote)
        while true ; do
            # Read file size (8 ASCII hex digits):
            ASCII_HEX_NUM_BYTES=$(dd status=none bs=1 count=8)
            NUM_BYTES=$((16#$ASCII_HEX_NUM_BYTES))
            dd status=none bs=1 count=$NUM_BYTES > $REMOTE_CLIPBOARD
            touch $TX_FORBIDDEN
            cat $REMOTE_CLIPBOARD | wl-copy # clipboard.sh
        done
        exit
        ;;
    *)
        USER_AT_HOST="$1"
        command -v ssh 1>/dev/null
        cat $0 | ssh -L $PORT:localhost:$PORT $USER_AT_HOST "
            killall ssy_sync_clipbo 2>/dev/null ;\
            killall wl-copy 2>/dev/null ;\
            killall wl-paste 2>/dev/null ;\
            killall nc 2>/dev/null ;\
            set -e ;\
            command -v wl-copy 1>/dev/null ;\
            command -v wl-paste 1>/dev/null ;\
            command -v nc 1>/dev/null ;\
            cat > /tmp/$(basename $0) ;\
            chmod +x /tmp/$(basename $0) ;\
            echo -e \"Now need to launch:\n$0 localhost\" ;\
            /tmp/$(basename $0) --server ;\
        "
        exit
        ;;
esac

if [ -z "$WAYLAND_DISPLAY" ]; then
    export WAYLAND_DISPLAY=wayland-0
fi

wl-paste --watch $0 --from-local | nc $NC_ARG localhost $PORT | $0 --from-remote
