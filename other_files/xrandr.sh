#!/bin/bash

# cvt 1920 1080 60
# xrandr --newmode $MAIN_MODE  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
# xrandr --addmode $MAIN_OUTPUT $MAIN_MODE

# xrandr --output --set underscan on
# xrandr --output --set "underscan hborder" 38
# xrandr --output --set "underscan vborder" 23

# xrandr --listproviders
# xrandr --setprovideroutputsource 4 0

# See also: /usr/share/doc/xserver-xorg-video-intel/xorg.conf

if [ "$1" = "udev" ]; then
    export DISPLAY=:0
    export DOTRC=/media/files/workspace/dotrc
    export DOTRC_S=/media/files/workspace/dotrc_s
    export HOME=/home/slava
    export XAUTHORITY=$HOME/.Xauthority
    sleep 0.5
fi

if [ -e $DOTRC_S/other_files/xrandr.sh ]; then
    . $DOTRC_S/other_files/xrandr.sh
fi

LOG=/tmp/log_xrandr_sh_$(id -u)

set_default()
{
    VAR_NAME=$1
    VALUE="$2"
    if [ -z "$(eval echo \$${VAR_NAME})" ]; then
        eval "$VAR_NAME=\"$VALUE\""
    fi
}

init_vars()
{
    echo "starting init_vars" >> $LOG
    # DisplayLink configuration adds new outputs to xrandr:
    NUM_PROVIDERS=$(xrandr --listproviders | head -n 1 | cut -d' ' -f4)
    echo "NUM_PROVIDERS=$NUM_PROVIDERS" >> $LOG
    if [ $NUM_PROVIDERS -eq 5 ]; then
        echo "executing xrandr --setprovideroutputsource" >> $LOG
        xrandr --setprovideroutputsource 3 0
        xrandr --setprovideroutputsource 4 0
    else
        echo "skipping xrandr --setprovideroutputsource" >> $LOG
    fi

    # eDP1 or eDP-1 on laptops:
    set_default CENTRAL_OUTPUT "$(xrandr | grep eDP | cut -d' ' -f1)"
    # set_default CENTRAL_MODE "--mode 1920x1080"
    # set_default CENTRAL_MODE "--auto"
    set_default CENTRAL_DPI "--dpi 144"
    # set_default CENTRAL_DPI "--dpi 96"

    set_default RIGHT_OUTPUT "$(xrandr | grep ' connected ' \
        | grep -v $CENTRAL_OUTPUT | head -n 1 | cut -d' ' -f1)"
    RIGHT_MODE="--auto"
    # RIGHT_DPI=

    if [ -n "$RIGHT_OUTPUT" ]; then
        set_default LEFT_OUTPUT "$(xrandr | grep ' connected ' \
            | grep -v $CENTRAL_OUTPUT | grep -v $RIGHT_OUTPUT | head -n 1 \
            | cut -d' ' -f1)"
        LEFT_MODE="--auto"
        # LEFT_MODE="--mode 1920x1080"
        # LEFT_DPI=
    fi

    # MAIN_OUTPUT has tray:
    set_default MAIN_OUTPUT "$CENTRAL_OUTPUT"

    echo "MAIN_OUTPUT=|$MAIN_OUTPUT|, CENTRAL_OUTPUT=|$CENTRAL_OUTPUT|" >> $LOG
    echo "RIGHT_OUTPUT=|$RIGHT_OUTPUT|, LEFT_OUTPUT=|$LEFT_OUTPUT|" >> $LOG
    echo "ending init_vars" >> $LOG
}

set -e
source $DOTRC/other_files/config_file.sh

update_i3_config()
{
    echo "starting update_i3_config()" >> $LOG
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
    echo "ending update_i3_config()" >> $LOG
}

xinitrc()
{
    echo -e "\nstarting xinitrc()" >> $LOG
    init_vars
    update_i3_config
    ARGS=""
    if [ -n "$LEFT_OUTPUT" ]; then
        ARGS="$ARGS --output $LEFT_OUTPUT $LEFT_MODE $LEFT_DPI \
            --left-of $CENTRAL_OUTPUT"
    else
        ARGS="$ARGS --pos 0x0"
    fi
    if [ -n "$RIGHT_OUTPUT" ]; then
        ARGS="$ARGS --output $RIGHT_OUTPUT $RIGHT_MODE $RIGHT_DPI \
            --right-of $CENTRAL_OUTPUT"
    fi
    CMD="xrandr --output $CENTRAL_OUTPUT $CENTRAL_MODE $CENTRAL_DPI $ARGS"
    echo "executing: xrandr -s 0" >> $LOG
    xrandr -s 0
    echo "executing: $CMD" >> $LOG
    $CMD
    echo "executing: i3-msg reload" >> $LOG
    i3-msg reload
    echo "ending xinitrc()" >> $LOG
}

if [ "$1" = "update_i3_config" ]; then
    echo "starting update_i3_config" >> $LOG
    init_vars
    update_i3_config
    echo "ending update_i3_config" >> $LOG
    exit
fi

if [ "$1" = "xinitrc" ]; then
    echo "starting xinitrc" >> $LOG
    xinitrc
    echo "ending xinitrc" >> $LOG
    exit
fi

xinitrc
