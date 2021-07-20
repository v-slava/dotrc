#!/bin/bash

TMP="/media/files/workspace/tmp"

set -ex
sudo apt-get install meson ninja-build pkg-config libpixman-1-dev libdrm-dev \
    libxkbcommon-dev libwayland-dev
mkdir -p "$TMP"
cd "$TMP"
rm -rf wayvnc neatvnc aml
git clone https://github.com/any1/wayvnc.git
git clone https://github.com/any1/neatvnc.git
git clone https://github.com/any1/aml.git

mkdir wayvnc/subprojects
cd wayvnc/subprojects
ln -s ../../neatvnc .
ln -s ../../aml .
cd -

mkdir neatvnc/subprojects
cd neatvnc/subprojects
ln -s ../../aml .
cd -

cd wayvnc
meson build
ninja -C build

# Run: build/wayvnc 0.0.0.0
