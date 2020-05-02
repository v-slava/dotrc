#!/usr/bin/env bash

set -exv

wget http://ftp.gnu.org/gnu/gdb/gdb-7.8.tar.xz
tar xf gdb-7.8.tar.xz
cd gdb-7.8/gdb/gdbserver
export LDFLAGS=" -static "
./configure --host=arm-v7a15a7v5r2-linux-gnueabi
make -j 9
cp gdbserver ../../../
