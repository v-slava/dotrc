#!/bin/bash

MAIN_OUTPUT=eDP1
MAIN_MODE=1920x1080

set -e

if [ "$1" = "xinitrc" ]; then
    xrandr --output $MAIN_OUTPUT --mode $MAIN_MODE
fi

MIRROR_MAIN=true
EXTERNAL_MODE=1920x1080

if [ "$MIRROR_MAIN" = "true" ]; then
    ARGS="--mode $MAIN_MODE"
else
    ARGS="--mode $EXTERNAL_MODE --right-of $MAIN_OUTPUT"
fi

OTHER_OUTPUTS="HDMI1 VGA1"

for OUTPUT in $OTHER_OUTPUTS ; do
    if xrandr | grep -q "$output connected" ; then
        xrandr --output $OUTPUT $ARGS
    else
        if [ "$1" = "external_monitor.sh" ]; then
            xrandr --output $OUTPUT --off
        fi
    fi
done
