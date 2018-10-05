#!/bin/bash

# sudo apt-get install busybox fakechroot

FAKE_ROOTFS=/tmp/fake_rootfs

# Create our fake root directory:
rm -rf $FAKE_ROOTFS
mkdir $FAKE_ROOTFS

# Create some obligatory subdirectories:
mkdir $FAKE_ROOTFS/bin
mkdir $FAKE_ROOTFS/sbin
mkdir $FAKE_ROOTFS/usr
mkdir $FAKE_ROOTFS/usr/bin
mkdir $FAKE_ROOTFS/usr/sbin

# Copy busybox binary:
cp /bin/busybox $FAKE_ROOTFS/bin/

# fakechroot chroot $FAKE_ROOTFS busybox sh -c busybox --install -s
fakechroot chroot $FAKE_ROOTFS busybox sh -c "busybox --install -s"

# Now execute:
# fakechroot chroot $FAKE_ROOTFS sh
