i3_msg()
(
    set -e
    local STDERR="$(i3-msg "$@" 2>&1 1>/dev/null)"
    # echo "i3_msg: REPLY = |$REPLY|"
    local RET=$?
    if [ $RET -ne 0 ]; then
        echo -e "Command:\ni3-msg "$@"\nfailed with exit code $RET.\n\
stderr:\n$STDERR" 1>&2
        exit $RET
    fi
    if [[ "$STDERR" =~ "ERROR:" ]]; then
        echo -e "Command:\ni3-msg "$@"\nfailed. stderr:\n$STDERR" 1>&2
        exit 1
    fi
)
