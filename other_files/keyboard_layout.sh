#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) {--toggle|US|RU}" 1>&2
    exit 1
}

INPUT_ID="1:1:AT_Translated_Set_2_keyboard"

case "$1" in
    --toggle)
        INPUT_DATA=$(swaymsg -t get_inputs | jq ".[] |
            select(.identifier==\"$INPUT_ID\")")
        INDEX=$(echo "$INPUT_DATA" | jq ".xkb_active_layout_index")
        LAYOUT_COUNT=$(echo "$INPUT_DATA" | jq ".xkb_layout_names | length")
        LAYOUT_IDX=$((($INDEX + 1) % $LAYOUT_COUNT))
        ;;
    US) LAYOUT_IDX=0 ;;
    RU) LAYOUT_IDX=1 ;;
    *) usage ;;
esac
swaymsg input "$INPUT_ID" xkb_switch_layout $LAYOUT_IDX
# Update keyboard layout (US|RU) in swaybar:
echo > /tmp/i3_status_fifo
