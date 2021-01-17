#!/bin/bash

# Prepare new guest user:

# useradd -c "guest user" -m -g 1000 -G plugdev -s /home/guest/guest_shell.sh guest
# sudo passwd guest
# sudo chmod 775 /home/guest
# mkdir -p /home/guest/devices/{0,1,2}
# sudo chmod -R 775 devices
# sudo chown -R guest:pi 775 devices
# openssl rand -base64 32 > /home/guest/devices/0/password
# openssl rand -base64 32 > /home/guest/devices/1/password
# openssl rand -base64 32 > /home/guest/devices/2/password
# sudo chown guest:pi /home/guest/devices/0/password /home/guest/devices/1/password /home/guest/devices/2/password
# sudo chmod 664 /home/guest/devices/0/password /home/guest/devices/1/password /home/guest/devices/2/password
#
# # Put this script to /home/guest/

GREETING="> "
DEVICES=$HOME/devices
export LD_LIBRARY_PATH=/usr/local/lib

ERR_INTERNAL=250
ERR_INVALID_PASSWORD=249
ERR_INVALID_CMD=248
ERR_INVALID_DEV_IDX=247

help()
{
    cat << EOF
update_password <device_index> <old_password> <new_password>
get_power_state <device_index> <password>
power_on <device_index> <password>
power_off <device_index> <password>
serial <device_index> <password>
exit_on_first_error <yes|no>
help
exit
EOF
}

lock()
{
    local WHAT="$1"
    shift
    local DEV_IDX="$1"
    shift
    local DIR="$DEVICES/$DEV_IDX/lock_$WHAT"
    if [ -d "$DIR" ]; then
        echo "Internal error: $DIR already exists" 1>&2
        exit $ERR_INTERNAL
    fi
    while ! mkdir "$DIR" ; do
        sleep 0.1
    done
    LOCK_DIR="$DIR"
}

unlock()
{
    if [ ! -d "$LOCK_DIR" ]; then
        echo "Internal error: $LOCK_DIR doesn't exist" 1>&2
        exit $ERR_INTERNAL
    fi
    rm -r "$LOCK_DIR"
    unset LOCK_DIR
}

cleanup()
{
    if [ -d "$LOCK_DIR" ]; then
        rm -r "$LOCK_DIR"
    fi
}

lock_auth()
{
    local WHAT="$1"
    shift
    local DEV_IDX="$1"
    shift
    local ACTUAL_PASSWORD="$1"
    shift
    local SHOULD_BE_EMPTY="$1"
    shift
    if [ -n "$SHOULD_BE_EMPTY" ]; then
        echo "Unexpected argument: $SHOULD_BE_EMPTY" 1>&2
        return $ERR_INVALID_CMD
    fi
    if [ -z "$DEV_IDX" ]; then
       echo "No device_index has been provided" 1>&2
       return $ERR_INVALID_CMD
    fi
    if ! [ "$DEV_IDX" -eq "$DEV_IDX" ] 2>/dev/null ; then
        echo "Invalid device index: $DEV_IDX" 1>&2
        return $ERR_INVALID_DEV_IDX
    fi
    local PASSWORD_FILE="$DEVICES/$DEV_IDX/password"
    if [ ! -f "$PASSWORD_FILE" ]; then
        echo "Invalid device index: $DEV_IDX" 1>&2
        return $ERR_INVALID_DEV_IDX
    fi
    lock "$WHAT" "$DEV_IDX"
    local EXPECTED_PASSWORD="$(cat "$PASSWORD_FILE")"
    if [ "$ACTUAL_PASSWORD" != "$EXPECTED_PASSWORD" ]; then
        sleep 0.1 # bruteforce protection
        echo "Invalid password" 1>&2
        unlock
        return $ERR_INVALID_PASSWORD
    fi
    return 0
}

execute_cmd()
{
    local CMD="$1"
    local DEV_IDX="$2"
    local PASSWORD="$3"
    local RET
    case "$CMD" in
        "update_password")
            local NEW_PASSWORD="$4"
            if [ -z "$NEW_PASSWORD" ]; then
                echo "<new_password> is missing" 1>&2
                return $ERR_INVALID_CMD
            fi
            lock_auth "password" "$DEV_IDX" "$PASSWORD" "$5"
            RET=$?
            if [ $RET -ne 0 ]; then
                return $RET
            fi
            echo "$NEW_PASSWORD" > "$DEVICES/$DEV_IDX/password"
            RET=$?
            unlock
            ;;
        "get_power_state")
            lock_auth "power" "$DEV_IDX" "$PASSWORD" "$4"
            RET=$?
            if [ $RET -ne 0 ]; then
                return $RET
            fi
            sispmctl -q -g $(($DEV_IDX + 1))
            RET=$?
            unlock
            ;;
        "power_on")
            lock_auth "power" "$DEV_IDX" "$PASSWORD" "$4"
            RET=$?
            if [ $RET -ne 0 ]; then
                return $RET
            fi
            sispmctl -q -o $(($DEV_IDX + 1))
            RET=$?
            unlock
            ;;
        "power_off")
            lock_auth "power" "$DEV_IDX" "$PASSWORD" "$4"
            RET=$?
            if [ $RET -ne 0 ]; then
                return $RET
            fi
            sispmctl -q -f $(($DEV_IDX + 1))
            RET=$?
            unlock
            ;;
        "serial")
            lock_auth "serial" "$DEV_IDX" "$PASSWORD" "$4"
            RET=$?
            if [ $RET -ne 0 ]; then
                return $RET
            fi
            minicom /dev/ttyUSB$DEV_IDX
            RET=$?
            # ser2net?
            unlock
            ;;
        "exit_on_first_error")
            if [ -n "$3" ]; then
                echo "Unexpected argument: $3" 1>&2
                return $ERR_INVALID_CMD
            fi
            case "$2" in
                "yes") EXIT_ON_FIRST_ERROR="yes" ; return 0 ;;
                "no") EXIT_ON_FIRST_ERROR="no" ; return 0 ;;
                "") echo "Expected argument: <yes|no>" 1>&2 ;;
                *) "Invalid argument: $2" 1>&2 ;;
            esac
            RET=$ERR_INVALID_CMD
            ;;
        "help")
            help
            RET=0
            ;;
        *)
            echo "Invalid command: $CMD" 1>&2
            RET=$ERR_INVALID_CMD
            ;;
    esac
    return $RET
}

main()
{
    echo -e "\nCommands:\n"
    help
    echo
    trap cleanup EXIT INT
    EXIT_ON_FIRST_ERROR="no" # default value
    local RET=0
    while true ; do
        echo -n "$GREETING"
        local LINE
        read LINE
        if [ $? -ne 0 ]; then
            break
        fi
        set -- $LINE
        if [ "$1" = "exit" ]; then
            break
        fi
        execute_cmd "$@"
        RET=$?
        if [ "$EXIT_ON_FIRST_ERROR" = "yes" ]; then
            if [ $RET -ne 0 ]; then
                break
            fi
        fi
    done
    return $RET
}

main "$@"
