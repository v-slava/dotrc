#!/usr/bin/env bash

set -exv

wget https://matt.ucc.asn.au/dropbear/releases/dropbear-2015.67.tar.bz2
tar xf dropbear-2015.67.tar.bz2
cd dropbear-2015.67

# export LDFLAGS=" -static " # There is no way to build it completely independent of glibc?
# warning: Using 'getspnam' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking

./configure --host=arm-v7a15a7v5r2-linux-gnueabi --disable-zlib
make -j 9
cp dropbear ../
# cp dropbearkey ../
# cp dropbearconvert ../
# cp dbclient ../

# /etc/dropbear/dropbear_dss_host_key
# /etc/dropbear/dropbear_rsa_host_key
# /etc/dropbear/dropbear_ecdsa_host_key
