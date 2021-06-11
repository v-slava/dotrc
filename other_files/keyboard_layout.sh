#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) {--get|--toggle|US|RU}" 1>&2
    exit 1
}

INPUT_ID="1:1:AT_Translated_Set_2_keyboard"

get_input_data_index()
{
    INPUT_DATA=$(swaymsg -t get_inputs | jq ".[] |
        select(.identifier==\"$INPUT_ID\")")
    INDEX=$(echo "$INPUT_DATA" | jq ".xkb_active_layout_index")
}

case "$1" in
    --get)
        get_input_data_index
        if [ $INDEX -eq 0 ]; then
            echo US
        else
            echo RU
        fi
        exit
        ;;
    --toggle)
        get_input_data_index
        LAYOUT_COUNT=$(echo "$INPUT_DATA" | jq ".xkb_layout_names | length")
        NEW_INDEX=$((($INDEX + 1) % $LAYOUT_COUNT))
        ;;
    US) NEW_INDEX=0 ;;
    RU) NEW_INDEX=1 ;;
    *) usage ;;
esac
swaymsg input "$INPUT_ID" xkb_switch_layout $NEW_INDEX
# Update keyboard layout (US|RU) in swaybar:
echo > /tmp/i3_status_fifo
