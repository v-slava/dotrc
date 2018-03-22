#!/bin/bash

MAIN_OUTPUT=eDP-1
MAIN_MODE=1920x1080
MAIN_DPI=144

set -e

if [ "$1" = "xinitrc" ]; then
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

EXTERNAL_MODE=1920x1080
EXTERNAL_DPI=144

if [ "$POSITION" = "--same-as" ]; then
    ARGS="$POSITION $MAIN_OUTPUT --mode $MAIN_MODE --dpi $MAIN_DPI"
else
    ARGS="$POSITION $MAIN_OUTPUT --mode $EXTERNAL_MODE --dpi $EXTERNAL_DPI"
fi

OTHER_OUTPUTS="HDMI-1 HDMI-2 DP-1 VGA-1"

for OUTPUT in $OTHER_OUTPUTS ; do
    if xrandr | grep -q "^$OUTPUT connected" ; then
        xrandr --output $OUTPUT $ARGS
    fi
    if xrandr | grep -q "^$OUTPUT disconnected" ; then
        xrandr --output $OUTPUT --off
    fi
done
