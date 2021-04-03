#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) {--up|--down|--off}" 1>&2
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

# For sway:
if [ "$1" = "--off" ]; then
    swaymsg 'output * dpms off'
    (
        sleep 1
        /media/files/programs/wait_for_input
        swaymsg 'output * dpms on'
    ) &
    exit 0
fi

BACKLIGHT=/sys/class/backlight/intel_backlight
CUR_BRIGHTNESS=$BACKLIGHT/brightness
MAX_BRIGHTNESS=$BACKLIGHT/max_brightness

CUR_VAL=$(cat $CUR_BRIGHTNESS)
MAX_VAL=$(cat $MAX_BRIGHTNESS)
CUR_PERCENTAGE=$(($CUR_VAL * 100 / $MAX_VAL))
DIFF_PERCENTAGE=$((CUR_PERCENTAGE / 10))
if [ "$DIFF_PERCENTAGE" -eq 0 ]; then
    DIFF_PERCENTAGE=1
fi

case "$1" in
    "--up") NEW_PERCENTAGE=$(($CUR_PERCENTAGE + $DIFF_PERCENTAGE)) ;;
    "--down") NEW_PERCENTAGE=$(($CUR_PERCENTAGE - $DIFF_PERCENTAGE)) ;;
    *) usage
esac

NEW_VAL=$(($MAX_VAL * $NEW_PERCENTAGE / 100))
if [ $NEW_VAL -gt $MAX_VAL ]; then
    NEW_VAL=$MAX_VAL
fi
echo $NEW_VAL | sudo tee $CUR_BRIGHTNESS
echo > /tmp/i3_status_fifo
exit

# For i3wm:
case "$1" in
    "--up") OP="-inc" ;;
    "--down") OP="-dec" ;;
    "--off") sleep 0.2 && xset dpms force off ; exit 0 ;;
    *) usage
esac

CUR_VAL=$(xbacklight -get | cut -d'.' -f1)
AMOUNT=$((CUR_VAL / 10))
if [ "$AMOUNT" -eq 0 ]; then
    AMOUNT=1
fi
xbacklight $OP $AMOUNT
# Warning: monitors from displaylink dock station don't work for me if default
# /etc/X11/xorg.conf is present.
# If xbacklight returns an error "No outputs have backlight property":
# 1,2) X :1 -configure # use this instead of following 1), 2).
# 1) Shutdown X server:
#    systemctl stop getty@tty1.service
#    i3-msg exit
# 2) Generate /etc/X11/xorg.conf:
#    X -configure
# 3) Put generated file to proper location:
#    mv ~/xorg.conf.new /etc/X11/xorg.conf
