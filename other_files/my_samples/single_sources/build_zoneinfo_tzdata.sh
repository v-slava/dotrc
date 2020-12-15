#!/bin/bash

# Console utility to select timezone: tzselect
#
# debian folders:
# - /usr/share/zoneinfo/posix (based on Coordinated Universal Time = UTC)
#   same as /usr/share/zoneinfo;
# - /usr/share/zoneinfo/right (based on International Atomic Time = TAI).
#   same as /usr/share/zoneinfo-leaps;
# - /usr/share/zoneinfo/Etc (for ships, without daylight saving).
#
# $ head -n1 /usr/share/zoneinfo/tzdata.zi
# # version 2020d
# $

OUTPUT="$(realpath "$1")"
set -e

if [ -z "$OUTPUT" ]; then
    echo "Usage: $(basename $0) path/to/zoneinfo.tar.xz" 1>&2
    exit 1
fi

if [ -f "$OUTPUT" ]; then
    exit 0 # nothing to do
fi

WORK_DIR="$(mktemp -d -t zoneinfo_build_dir_$(date '+%Y_%m_%d__%H_%M_%S_%N')_XXXXXXXXXX)"
TMP_OUTPUT="$WORK_DIR/zoneinfo.tar.xz"
OUT_DIR="$WORK_DIR/out" # mimics /usr/share/zoneinfo
TZ_DIR="$WORK_DIR/tz"
VERSION=2020d # see version tags: https://github.com/eggert/tz/tags
git -C "$WORK_DIR" clone --branch $VERSION --depth 1 https://github.com/eggert/tz.git
make -C "$TZ_DIR" zic tzdata.zi # leapseconds
# make -C "$TZ_DIR"
# make -C "$TZ_DIR" TOPDIR="$OUT_DIR" install
# make -C "$TZ_DIR" TOPDIR="$OUT_DIR" install_data
"$TZ_DIR/zic" -d "$OUT_DIR" -L /dev/null "$TZ_DIR/tzdata.zi"
# "$TZ_DIR/zic" -d "$OUT_DIR/right" -L "$TZ_DIR/leapseconds" "$TZ_DIR/tzdata.zi"
grep '^L ' "$TZ_DIR/tzdata.zi" | while read L TARGET LINK_NAME ; do
    ln -sf "$OUT_DIR/$TARGET" "$OUT_DIR/$LINK_NAME"
done
symlinks -r -s -c "$OUT_DIR"
cd "$OUT_DIR"
tar cfJ "$TMP_OUTPUT" .
mv "$TMP_OUTPUT" "$OUTPUT"
