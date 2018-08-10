#!/bin/bash

MAIN_OUTPUT=eDP1
MAIN_MODE=1920x1080
MAIN_DPI=144

EXTERNAL_MODE=1920x1080
EXTERNAL_DPI=96

# multimonitor configuration:
CENTRAL_OUTPUT=DP-2-1
CENTRAL_MODE=1920x1080
CENTRAL_DPI=96

RIGHT_OUTPUT=$MAIN_OUTPUT
RIGHT_MODE=$MAIN_MODE
RIGHT_DPI=$MAIN_DPI

LEFT_OUTPUT=DP-2-2
LEFT_MODE=1920x1080
LEFT_DPI=96

set -e

update_i3_config()
{
    I3_CONF=.config_xdg/i3/config
    $DOTRC/other_files/update_config.sh $I3_CONF
    I3_CONF=~/$I3_CONF
    if xrandr | grep -q "^$CENTRAL_OUTPUT connected" &&
            xrandr | grep -q "^$LEFT_OUTPUT connected" &&
            xrandr | grep -q "^$RIGHT_OUTPUT connected" ; then
        sed -i $I3_CONF -e "s/OUTPUT_TRAY_TEMPLATE/$CENTRAL_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_1_TEMPLATE/$LEFT_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_2_TEMPLATE/$CENTRAL_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_3_TEMPLATE/$RIGHT_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_4_TEMPLATE/$LEFT_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_5_TEMPLATE/$CENTRAL_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_6_TEMPLATE/$RIGHT_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_7_TEMPLATE/$LEFT_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_8_TEMPLATE/$CENTRAL_OUTPUT/g"
        sed -i $I3_CONF -e "s/OUTPUT_9_TEMPLATE/$RIGHT_OUTPUT/g"
    else
        # Need to use single monitor configuration.
        sed -i $I3_CONF -e "s/OUTPUT_TRAY_TEMPLATE/$MAIN_OUTPUT/g"
        for i in $(seq 1 9) ; do
            sed -i $I3_CONF -e "s/OUTPUT_${i}_TEMPLATE/$MAIN_OUTPUT/g"
        done
    fi
    sed -i $I3_CONF -e "s|DOTRC_TEMPLATE|$DOTRC|g"
    sed -i $I3_CONF -e "s|DOTRC_S_TEMPLATE|$DOTRC_S|g"
}

if [ "$1" = "update_configs" ]; then
    update_i3_config
    exit
fi

if [ "$1" = "xinitrc" ]; then
    update_i3_config
    xrandr --output $MAIN_OUTPUT --mode $MAIN_MODE --dpi $MAIN_DPI
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
    ARGS="$POSITION $MAIN_OUTPUT --mode $MAIN_MODE --dpi $MAIN_DPI"
else
    ARGS="$POSITION $MAIN_OUTPUT --mode $EXTERNAL_MODE --dpi $EXTERNAL_DPI"
fi

OTHER_OUTPUTS="HDMI1 HDMI2 DP1 VGA1"

for OUTPUT in $OTHER_OUTPUTS ; do
    if xrandr | grep -q "^$OUTPUT connected" ; then
        xrandr --output $OUTPUT $ARGS
    fi
    if xrandr | grep -q "^$OUTPUT disconnected" ; then
        xrandr --output $OUTPUT --off
    fi
done
