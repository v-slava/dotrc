#!/bin/bash

TMP=/media/files/workspace/tmp
INITRAMFS_DIR=$TMP/initramfs
# INITRAMFS_IMG=$TMP/initramfs.igz
INITRAMFS_IMG=$TMP/initramfs.cpio

set -ex
mkdir -p "$TMP"
cd "$TMP"
sudo apt-get install gcc-arm-linux-gnueabihf libc-dev-armhf-cross
rm -f $INITRAMFS_IMG
rm -rf $INITRAMFS_DIR
rm -rf busybox
git clone https://git.busybox.net/busybox/
cd busybox
# git clean -fdx

export ARCH=arm
export CROSS_COMPILE=arm-linux-gnueabihf-

# make allyesconfig
make defconfig
# WARNING: toolchain may link THUMB code into binary which can lead to errors
# on target (if doesn't support THUMB) like:
# Starting init: /sbin/init exists but couldn't execute it (error -8)
# Linux config to add THUMB support: CONFIG_ARM_THUMB=y
# Another error:
# Kernel panic - not syncing: Attempted to kill init! exitcode=0x00000004
# To avoid it turn use linux config: CONFIG_VFP=y
# Another error:
# process 27 (getty) attempted a POSIX timer syscall while CONFIG_POSIX_TIMERS
# is not set
sed -i .config \
    -e 's/^# CONFIG_STATIC is not set$/CONFIG_STATIC=y/g' \

    # -e 's/^CONFIG_EXTRA_CFLAGS=""$/CONFIG_EXTRA_CFLAGS="-marm"/g' \

# grep -q 'CONFIG_STATIC=y' .config
# make -j9 menuconfig
make -j9
# make V=1 > /tmp/busybox_build_log

mkdir -p $INITRAMFS_DIR/etc/init.d
echo 'root::0:0:root:/root:/bin/sh' > $INITRAMFS_DIR/etc/passwd
echo 'root:x:0:' > $INITRAMFS_DIR/etc/group
cat << EOF > $INITRAMFS_DIR/etc/inittab
::sysinit:/etc/init.d/rcS
# ::respawn:/sbin/getty -L 115200 ttyS0
::respawn:/sbin/getty -L 115200 ttymxc0
EOF
cat << EOF > $INITRAMFS_DIR/etc/init.d/rcS
#!/bin/sh
mkdir -p /dev
mount -t devtmpfs none /dev
# The following is not strictly necessary for /sbin/getty or /bin/sh to work:
# mkdir -p /proc
# mount -t proc none /proc
# mkdir -p /sys
# mount -t sysfs none /sys
# mkdir -p /dev/pts
# mount -t devpts none /dev/pts # In linux kernel: CONFIG_UNIX98_PTYS=y
# hostname sw
EOF
chmod +x $INITRAMFS_DIR/etc/init.d/rcS

# cat << EOF > /tmp/hello.c
# #include <stdio.h>
# int main(void) {
#     puts("hello");
#     return 0;
# }
# EOF
# ${CROSS_COMPILE}gcc /tmp/hello.c -o $INITRAMFS_DIR/hello.c.out
# # Note: not a static compilation, command:
# # readelf -d $INITRAMFS_DIR/hello.c.out | grep NEEDED
# # reports that only libc.so.6 is required.
# # The following allows to run hello.c.out:
# mkdir $INITRAMFS_DIR/lib
# LIB=/usr/arm-linux-gnueabihf/lib
# cp -d $LIB/ld-linux-armhf.so.3 $INITRAMFS_DIR/lib/
# cp -d $(realpath $LIB/ld-linux-armhf.so.3) $INITRAMFS_DIR/lib/
# cp -d $LIB/libc.so.6 $INITRAMFS_DIR/lib/
# cp -d $(realpath $LIB/libc.so.6) $INITRAMFS_DIR/lib/
# # The following additionally allows busybox to work without CONFIG_STATIC=y:
# cp -d $LIB/libm.so.6 $INITRAMFS_DIR/lib/
# cp -d $(realpath $LIB/libm.so.6) $INITRAMFS_DIR/lib/
# cp -d $LIB/libresolv.so.2 $INITRAMFS_DIR/lib/
# cp -d $(realpath $LIB/libresolv.so.2) $INITRAMFS_DIR/lib/

make CONFIG_PREFIX=$INITRAMFS_DIR install
cd $INITRAMFS_DIR
find | cpio -o -H newc -R root:root > $INITRAMFS_IMG # | gzip -9
# In linux kernel configuration:
# CONFIG_INITRAMFS_SOURCE="$INITRAMFS_IMG"
# Without this config we must use:
# bootz 0x85000000 $INITRAMFS_IMAGE_ADDR 0x84000000
# But uboot may be misconfigured and report:
#
# Wrong Ramdisk Image Format
# Ramdisk image is corrupt or invalid
#
# So better stick to CONFIG_INITRAMFS_SOURCE.
#
# To debug early userspace (/sbin/init), use the following kernel command line
# argument: init=/my_script.sh
# Make /my_script.sh executable, mount /dev, echo some_info > /dev/YOUR_TTY,
# pat attention to exit code of /my_script.sh that linux prints.
#
# In uboot command line:
# setenv bootargs 'console=ttymxc0,115200' && tftpboot 0x85000000
# SOME_IP:/zImage && tftpboot 0x84000000 SOME_IP:/1.dtb && bootz 0x85000000 - 0x84000000
#
# Note: zImage must be put within first 128M of physical memory, decompression
# stub will relocate it to the start of physical memory. In this example
# physical memory is [0x80000000; 0xA0000000) (see device tree of your board).
# Minimal DTB:
# /dts-v1/;
# #include "imx6ull.dtsi"
# &uart1 {
#     status = "okay";
# };
