#!/bin/bash

LLVM_VERSION=11

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
TMP="/media/files/workspace/tmp"
set -ex
sudo apt-get install cmake ninja-build libclang-dev llvm-dev clang
# sudo apt-get install cmake ninja-build libclang-${LLVM_VERSION}-dev \
#     llvm-${LLVM_VERSION}-dev clang-${LLVM_VERSION}
mkdir -p "$TMP"
cd "$TMP"
rm -rf rtags
git clone --recursive https://github.com/Andersbakken/rtags.git
git -C rtags tag
git -C rtags checkout master # git -C rtags checkout v3.23
rm -rf rtags_out
mkdir rtags_out
cd rtags_out
export CC=/usr/bin/clang-${LLVM_VERSION}
export CXX=/usr/bin/clang++-${LLVM_VERSION}
cmake -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Release \
    ../rtags
ninja
sudo ninja install
