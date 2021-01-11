#!/bin/bash

set -e

# https://support.displaylink.com/knowledgebase/articles/679060
DOWNLOADS=/media/files/downloads
ZIP="$(ls $DOWNLOADS/DisplayLink\ USB\ Graphics\ Software\ for\ Ubuntu\ *.zip)"
if [ ! -f "$ZIP" ]; then
    echo "Please download displaylink installer first" 1>&2
    x-www-browser https://www.displaylink.com/downloads/ubuntu
    exit 1
fi
TMP_DIR=/tmp/displaylink_installer
rm -rf $TMP_DIR
mkdir $TMP_DIR
cd $TMP_DIR
unzip "$ZIP"
RUN=$(ls)
bash $RUN --noexec --keep
DIR=$(basename -s .run $RUN)
cd $DIR
CUR_DIR=$PWD
mv evdi.tar.gz orig_evdi.tar.gz
if [ ! -d $DOWNLOADS/evdi ]; then
    git -C $DOWNLOADS clone https://github.com/DisplayLink/evdi.git
fi
VERSION=origin/devel # v1.9.0
git -C $DOWNLOADS/evdi archive $VERSION -o $CUR_DIR/evdi.tar.gz
if lsmod | grep -q evdi ; then
    CMD=uninstall
else
    CMD=install
fi
set -x
sudo ./displaylink-installer.sh $CMD
