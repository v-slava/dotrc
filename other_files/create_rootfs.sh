#!/bin/bash

# sudo apt-get install busybox fakechroot

FAKE_ROOTFS=/tmp/fake_rootfs

# Create our fake root directory:
rm -rf $FAKE_ROOTFS
mkdir $FAKE_ROOTFS

# Create some obligatory subdirectories:
mkdir -p $FAKE_ROOTFS/{bin,sbin,usr/{bin,sbin}}

# Copy busybox binary:
cp /bin/busybox $FAKE_ROOTFS/bin/

if [ false ]; then
    # Create fake chroot environment for unit tests:
    # Note: the busybox should be dynamic

    fakechroot chroot $FAKE_ROOTFS busybox sh -c "busybox --install -s"

    # Now execute:
    # fakechroot chroot $FAKE_ROOTFS sh

else
    # Create minimal rootfs for qemu-system-x86_64
    # Note: the busybox should be static

    busybox --list-full | while read file ; do
        ln -s /bin/busybox $FAKE_ROOTFS/$file
    done

    mkdir -p $FAKE_ROOTFS/{proc,sys,dev,etc/init.d}
    cat << EOF > $FAKE_ROOTFS/etc/init.d/rcS
#!/bin/sh

mount -t proc none /proc
mount -t sysfs none /sys
/sbin/mdev -s
EOF
    chmod +x $FAKE_ROOTFS/etc/init.d/rcS

    CUR_DIR=$PWD
    cd $FAKE_ROOTFS && find | cpio -o -H newc | gzip -9 > $CUR_DIR/initramfs.igz
    cd -
fi


