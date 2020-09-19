#!/bin/bash

/usr/sbin/minidlnad \
    -f $DOTRC/other_files/minidlna.conf \
    -P /tmp/minidlna/pid \
    -R -S -L
