#!/bin/bash

BUILD_DIR=$PWD/../cmake_demo_out
SRC_ROOT_DIR=$PWD

if [[ -d $BUILD_DIR ]]; then
    rm -rf $BUID_DIR
fi

mkdir -p $BUILD_DIR
cd $BUILD_DIR
cmake $SRC_ROOT_DIR -DCMAKE_BUILD_TYPE=Debug
make
echo -e "\n\n"
$BUILD_DIR/exec_src/demo_exec
