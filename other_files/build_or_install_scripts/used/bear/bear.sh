#!/bin/bash

set -e

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PATCH="$THIS_DIR/bear.patch"
TMP="$WORKSPACE/tmp"
# PROGRAMS="/media/files/programs"
# BEAR_OUT="$PROGRAMS/bear"

set -x
# rm -rf "$BEAR_OUT"
# mkdir -p "$BEAR_OUT"
mkdir -p "$TMP"
cd "$TMP"
rm -rf Bear
git clone https://github.com/rizsotto/Bear
cd Bear
git checkout 2.4.2
git am < "$PATCH"
mkdir out
cd out
# -DCMAKE_INSTALL_PREFIX="$BEAR_OUT"
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j5
su -c 'make -j5 install'
