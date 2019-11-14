#!/bin/sh

cd "$(dirname "$0")" || exit 1

./qemu.sh
sleep 2
exec ./spice.sh
