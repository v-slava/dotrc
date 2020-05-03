#!/bin/bash

usage()
{
    echo "Usage: $(basename $0) {--up|--down}" 1>&2
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

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
# If xbacklight returns an error "No outputs have backlight property" on ASUS
# X541UA laptop, we need to do the following:
# 1) Shutdown X server:
#    systemctl stop getty@tty1.service
#    i3-msg exit
# 2) Generate /etc/X11/xorg.conf:
#    X -configure
# 3) Put generated file to proper location:
#    mv ~/xorg.conf.new /etc/X11/xorg.conf
