#!/bin/bash

TMP="/media/files/workspace/tmp"

set -ex
sudo apt-get install libssl-dev cmake pkg-config
mkdir -p "$TMP"
cd "$TMP"
rm -rf Bear
git clone https://github.com/rizsotto/Bear
cd Bear
git checkout 3.0.13
mkdir out
cd out
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j5
sudo make -j5 install
