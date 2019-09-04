#!/bin/bash

MAIN_OUTPUT=eDP1
MAIN_MODE="--mode 1920x1080"
MAIN_DPI="--dpi 96"
# MAIN_DPI="--dpi 96"

EXTERNAL_MODE="--mode 1920x1080"
# EXTERNAL_DPI="--dpi 144"

# multimonitor configuration:
CENTRAL_OUTPUT=DP-2-1
CENTRAL_MODE="--mode 1920x1080"
# CENTRAL_DPI=

RIGHT_OUTPUT=$MAIN_OUTPUT
RIGHT_MODE=$MAIN_MODE
RIGHT_DPI=$MAIN_DPI

LEFT_OUTPUT=DP-2-2
LEFT_MODE="--mode 1920x1080"
# LEFT_DPI="--dpi 144"

set -e
source ~/.bashrc
source $DOTRC/other_files/config_file.sh

update_i3_config()
{
    I3_CONF=.config_xdg/i3/config
    config_concat_dotrc_s $I3_CONF
    config_fix_env_vars $I3_CONF DOTRC DOTRC_S
    I3_CONF=~/$I3_CONF
    XRANDR="$(xrandr)"
    if echo -e "$XRANDR" | grep -q "^$CENTRAL_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$LEFT_OUTPUT connected" &&
        echo -e "$XRANDR" | grep -q "^$RIGHT_OUTPUT connected" ; then
        sed -i $I3_CONF \
            -e "s/OUTPUT_TRAY_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_1_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_2_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_3_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_4_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_5_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_6_TEMPLATE/$RIGHT_OUTPUT/g" \
            -e "s/OUTPUT_7_TEMPLATE/$LEFT_OUTPUT/g" \
            -e "s/OUTPUT_8_TEMPLATE/$CENTRAL_OUTPUT/g" \
            -e "s/OUTPUT_9_TEMPLATE/$RIGHT_OUTPUT/g" \

    else
        # Need to use single monitor configuration.
        sed -i $I3_CONF -e "s/OUTPUT_TRAY_TEMPLATE/$MAIN_OUTPUT/g"
        for i in $(seq 1 9) ; do
            sed -i $I3_CONF -e "s/OUTPUT_${i}_TEMPLATE/$MAIN_OUTPUT/g"
        done
    fi
    sed -i $I3_CONF \
        -e "s|DOTRC_TEMPLATE|$DOTRC|g" \
        -e "s|DOTRC_S_TEMPLATE|$DOTRC_S|g" \

}

if [ "$1" = "update_i3_config" ]; then
    update_i3_config
    exit
fi

if [ "$1" = "xinitrc" ]; then
    update_i3_config

    # cvt 1920 1080 60
    # xrandr --newmode $MAIN_MODE  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
    # xrandr --addmode $MAIN_OUTPUT $MAIN_MODE

    xrandr --output $MAIN_OUTPUT $MAIN_MODE $MAIN_DPI

#     xrandr --output $CENTRAL_OUTPUT $CENTRAL_MODE $CENTRAL_DPI \
# --output $RIGHT_OUTPUT $RIGHT_MODE $RIGHT_DPI --right-of $CENTRAL_OUTPUT \
# --output $LEFT_OUTPUT $LEFT_MODE $LEFT_DPI --left-of $CENTRAL_OUTPUT

    exit
fi

POSITION=$1
case "$POSITION" in
    "--left-of") ;;
    "--right-of") ;;
    "--same-as") ;;
    *)
        echo "Unexpected argument: $1" 1>&2
        exit 1
esac

if [ "$POSITION" = "--same-as" ]; then
    ARGS="$POSITION $MAIN_OUTPUT $MAIN_MODE $MAIN_DPI"
else
    ARGS="$POSITION $MAIN_OUTPUT $EXTERNAL_MODE $EXTERNAL_DPI"
fi

OTHER_OUTPUTS="HDMI1 HDMI2 DP1 VGA1 VGA-2"

for OUTPUT in $OTHER_OUTPUTS ; do
    if xrandr | grep -q "^$OUTPUT connected" ; then
        xrandr --output $OUTPUT $ARGS
    fi
    if xrandr | grep -q "^$OUTPUT disconnected" ; then
        xrandr --output $OUTPUT --off
    fi
done
