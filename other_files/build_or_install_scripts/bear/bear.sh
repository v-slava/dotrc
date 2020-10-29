#!/bin/bash

set -e

TMP="/media/files/workspace/tmp"

set -x
mkdir -p "$TMP"
cd "$TMP"
rm -rf Bear
git clone https://github.com/rizsotto/Bear
cd Bear
git checkout 3.0.1
mkdir out
cd out
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j5
sudo make -j5 install
