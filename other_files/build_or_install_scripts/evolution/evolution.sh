#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PATCH="$THIS_DIR/hotkey.patch"
TMP="/media/files/workspace/tmp"
set -ex
sudo apt-get purge evolution libevolution
sudo apt-get install evolution-common evolution-data-server evolution-data-server-common
sudo apt-get build-dep evolution
mkdir -p "$TMP"
cd "$TMP"
rm -rf evolution
git clone https://gitlab.gnome.org/GNOME/evolution.git
cd evolution
git checkout 3.38.2
git apply "$PATCH"
mkdir out
cd out
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j9
sudo make -j9 install
