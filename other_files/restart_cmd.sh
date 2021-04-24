#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) [-v] CMD" 1>&2
    # Examples:
    # $DOTRC/other_files/restart_cmd.sh -v $DOTRC_S/other_files/vpn.sh --restart
    exit 1
}

if [ "$1" = "-v" ]; then
    VERBOSE=true
    shift
fi
CMD="$1"
if [ -z "$CMD" ]; then
    usage
fi

MSG_COLOR="\e[0;36m" # cyan
COLOR_END="\e[m"

if [ -n "$VERBOSE" ]; then
    echo -e "${MSG_COLOR}Starting command:\n$CMD\n$COLOR_END"
fi

while true ; do
    $CMD
    RET=$?
    if [ $RET -eq 0 ]; then
        COLOR="\e[0;32m" # green
    else
        COLOR="\e[0;31m" # red
    fi
    if [ -n "$VERBOSE" ]; then
        echo -e "$MSG_COLOR\nCommand:\n$CMD"
    else
        echo -en "$MSG_COLOR\nCommand "
    fi
    echo -e "${COLOR}exited with code: $RET.$MSG_COLOR"
    echo -e "Press 'q' to quit or any other key to restart the command..."
    read -n1 SYMBOL
    if [ "$SYMBOL" = "q" ]; then
        echo -e "$COLOR\nExiting with code: $RET...$COLOR_END"
        break
    fi
    echo -e "\nRestarting the command...\n$COLOR_END"
done

exit $RET
