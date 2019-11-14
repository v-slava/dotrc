#!/usr/bin/env bash

set -ex

sudo apt-get install libgtest-dev
mkdir /tmp/gtest
cd /tmp/gtest
CXXFLAGS=-fPIC cmake -DCMAKE_BUILD_TYPE=Release /usr/src/gtest
make
sudo cp *.a /usr/lib/
