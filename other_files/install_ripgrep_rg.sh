#!/usr/bin/env bash

VERSION=11.0.1
FILE=ripgrep_${VERSION}_amd64.deb
set -ex
wget https://github.com/BurntSushi/ripgrep/releases/download/$VERSION/$FILE
dpkg -i $FILE
rm $FILE
