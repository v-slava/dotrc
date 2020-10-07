#!/bin/bash

# cvt 1920 1080 60
# xrandr --newmode $MAIN_MODE  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
# xrandr --addmode $MAIN_OUTPUT $MAIN_MODE

# xrandr --output --set underscan on
# xrandr --output --set "underscan hborder" 38
# xrandr --output --set "underscan vborder" 23

if [ -e $DOTRC_S/other_files/xrandr.sh ]; then
    . $DOTRC_S/other_files/xrandr.sh
fi

set_default()
{
    VAR_NAME=$1
    VALUE="$2"
    if [ -z "$(eval echo \$${VAR_NAME})" ]; then
        eval "$VAR_NAME=\"$VALUE\""
    fi
}

# eDP1 or eDP-1 on laptops:
set_default CENTRAL_OUTPUT "$(xrandr | grep eDP | cut -d' ' -f1)"
set_default CENTRAL_MODE "--mode 1920x1080"
set_default CENTRAL_DPI "--dpi 144"
# set_default CENTRAL_DPI "--dpi 96"

set_default RIGHT_OUTPUT "$(xrandr | grep -v eDP | grep ' connected ' \
    | head -n 1 | cut -d' ' -f1)"
# RIGHT_MODE=
# RIGHT_DPI=

# LEFT_OUTPUT=
# LEFT_MODE=
# LEFT_DPI=

# MAIN_OUTPUT has tray:
set_default MAIN_OUTPUT "$CENTRAL_OUTPUT"

set -e
source $DOTRC/other_files/config_file.sh

update_i3_config()
{
    FILE=".config_xdg/i3/config"
    DOTRC_FILE="$DOTRC/home_settings/$FILE"
    DOTRC_S_FILE="$DOTRC_S/home_settings/$FILE"
    DEST_DIR="$HOME/"
    DEST="${DEST_DIR}$FILE"
    config_concat_dotrc_s
    config_fix_env_vars DOTRC DOTRC_S

    I3_CONF=~/$FILE
    XRANDR="$(xrandr)"
    sed -i $I3_CONF -e "s/OUTPUT_TRAY_TEMPLATE/$MAIN_OUTPUT/g"
    if echo -e "$XRANDR" | grep -q "^$CENTRAL_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$LEFT_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$RIGHT_OUTPUT connected" ; then
        sed -i $I3_CONF \
            -e "s/OUTPUT_1_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_2_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_3_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_4_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_5_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_6_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_7_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_8_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_9_TEMPLATE/$RIGHT_OUTPUT/g" \

    elif echo -e "$XRANDR" | grep -q "^$CENTRAL_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$LEFT_OUTPUT connected" ; then
        sed -i $I3_CONF \
            -e "s/OUTPUT_1_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_2_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_3_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_4_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_5_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_6_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_7_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_8_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_9_TEMPLATE/$CENTRAL_OUTPUT/g" \

    elif echo -e "$XRANDR" | grep -q "^$CENTRAL_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$RIGHT_OUTPUT connected" ; then
        sed -i $I3_CONF \
            -e "s/OUTPUT_1_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_2_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_3_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_4_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_5_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_6_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_7_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_8_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_9_TEMPLATE/$CENTRAL_OUTPUT/g" \

    else
        # Need to use single monitor configuration.
        for i in $(seq 1 9) ; do
            sed -i $I3_CONF -e "s/OUTPUT_${i}_TEMPLATE/$CENTRAL_OUTPUT/g"
        done
    fi
}

if [ "$1" = "update_i3_config" ]; then
    update_i3_config
    exit
fi

if [ "$1" = "xinitrc" ]; then
    update_i3_config
    ARGS=""
    if [ -n "$LEFT_OUTPUT" ]; then
        ARGS="$ARGS --output $LEFT_OUTPUT $LEFT_MODE $LEFT_DPI \
            --left-of $CENTRAL_OUTPUT"
    fi
    if [ -n "$RIGHT_OUTPUT" ]; then
        ARGS="$ARGS --output $RIGHT_OUTPUT $RIGHT_MODE $RIGHT_DPI \
            --right-of $CENTRAL_OUTPUT"
    fi
    set -x
    xrandr --output $CENTRAL_OUTPUT $CENTRAL_MODE $CENTRAL_DPI $ARGS
    exit
fi
