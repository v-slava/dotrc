#!/bin/bash

# In $XDG_CONFIG_HOME/i3/config use:
# bindsym --release $Alt_L+i exec --no-startup-id exec $START $DOTRC/other_files/send_input.sh

SEND_INPUT_DIR=$DOTRC_S/other_files/send_input
CONFIG_FILE=$SEND_INPUT_DIR/config

# Note: "xdotool type" doesn't like double quotes.
# Example contents of $CONFIG_FILE
# n	new password	xdotool type "$(echo -e "echo new\ \ password\r\n")"
# o	old password	$DOTRC_S/other_files/send_input/old_password.sh
# t	test	xdotool type "echo hello" && xdotool key Return # some comment
# t	test	sudo ydotool type "echo hello" && sudo ydotool key Enter

set -e

if [ ! -e $CONFIG_FILE ]; then
    echo "$CONFIG_FILE doesn't exist" 1>&2
    exit 1
fi

NUM_LINES=$(cat $CONFIG_FILE | wc -l)

case $NUM_LINES in
    0) echo "$CONFIG_FILE is empty" 1>&2 ; exit 1 ;;
    1) LINE="$(cat $CONFIG_FILE)" ;;
    *)
    if true ; then
        FILTER_DMENU="dmenu -p Type: -l $NUM_LINES"
        FILTER_ROFI="rofi -theme Monokai -format d -dmenu -auto-select \
            -lines $NUM_LINES -p Type"
        FILTER="$FILTER_ROFI"
        FILTER="$FILTER_DMENU"

        set +e
        INPUT="$(cat $CONFIG_FILE | cut -f 2 | $FILTER )"
        set -e
        if [ -z "$INPUT" ]; then
            echo "Nothing has been selected" 1>&2
            # exit 1
            exit 0
        fi

        if [ "$FILTER" == "$FILTER_ROFI" ]; then
            LINE="$(sed "${INPUT}q;d" $CONFIG_FILE)"
            sleep 0.1 # without this we loose some input..
        else
            LINE="$(grep "^..$INPUT" $CONFIG_FILE | head -n1)"
        fi
    else
        INPUT_FILE=/tmp/send_input_symbol
        rm -f $INPUT_FILE

        SCRIPT="\
        echo -e \"Select what to type (press single character):\n\" \
        && cat $CONFIG_FILE | cut -f -2 | sed -e 's|\t| - |g' \
        && echo -en \"\nYour choice: \" \
        && read -n1 INPUT_SYMBOL \
        && echo -n \$INPUT_SYMBOL > $INPUT_FILE"

        $DOTRC/other_files/open_terminal.sh --title "Send input" bash -c "$SCRIPT"

        set +e
        LINE="$(grep "^$(cat $INPUT_FILE)" $CONFIG_FILE | head -n1)"
        set -e
        if [ -z "$LINE" ]; then
            echo "Invalid input" 1>&2
            exit 1
        fi
        sleep 0.1 # without this we loose some input..
    fi
    ;;
esac

CMD="$(echo "$LINE" | cut -f3)"
bash -c "$CMD"
