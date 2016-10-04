#!/bin/bash

ROOT_PARTITION=sdb1
ROOT_MOUNT_POINT=/media/root_mnt
DISTRO_NAME=jessie

set -ex

apt-get install vim
apt-get install debootstrap
mkfs -t ext4 -L root_partition /dev/$ROOT_PARTITION

rm -rf $ROOT_MOUNT_POINT
mkdir $ROOT_MOUNT_POINT
mount /dev/$ROOT_PARTITION $ROOT_MOUNT_POINT
debootstrap --arch=amd64 --variant=minbase $DISTRO_NAME $ROOT_MOUNT_POINT

cat << EOF > $ROOT_MOUNT_POINT/etc/apt/apt.conf
APT::Install-Recommends "0";
APT::Install-Suggests "0";
EOF

cat << EOF > $ROOT_MOUNT_POINT/etc/apt/sources.list
# Package search:
# https://www.debian.org/distrib/packages#search_packages

# Security updates:
deb http://security.debian.org/ jessie/updates main contrib non-free
deb-src http://security.debian.org/ jessie/updates main contrib non-free

# Base repository:
deb http://http.debian.net/debian jessie main contrib non-free
deb-src http://http.debian.net/debian jessie main contrib non-free

# Stable updates:
deb http://http.debian.net/debian jessie-updates main contrib non-free
deb-src http://http.debian.net/debian jessie-updates main contrib non-free

# Stable backports:
deb http://http.debian.net/debian jessie-backports main contrib non-free
deb-src http://http.debian.net/debian jessie-backports main contrib
EOF

cat << EOF > $ROOT_MOUNT_POINT/etc/fstab
# <file system>        <dir>         <type>    <options>           <dump> <pass>
/dev/sda3              /             ext4      defaults,noatime      0      1
EOF

mkdir -p $ROOT_MOUNT_POINT/etc/X11/native
mkdir -p $ROOT_MOUNT_POINT/etc/X11/virtual

mount -B /sys $ROOT_MOUNT_POINT/sys
mount -B /proc $ROOT_MOUNT_POINT/proc
mount -B /dev $ROOT_MOUNT_POINT/dev

cat << EOF > $ROOT_MOUNT_POINT/root/install.sh
#!/bin/bash

set -ex

apt-get update
apt-get upgrade

passwd

# apt-get install linux-image-amd64 grub-pc os-prober
apt-get install -t jessie-backports linux-image-4.7.0-0.bpo.1-amd64-unsigned grub-pc os-prober

apt-get install -t jessie-backports firmware-linux-free firmware-linux-nonfree xserver-xorg-video-radeon xorg xserver-xorg-video-radeon xserver-xorg-input-evdev xinit rxvt-unicode libgl1-mesa-dri

apt-get install -t jessie-backports i3-wm
apt-get install -t jessie-backports mesa-utils
apt-get install -t jessie-backports build-essential
apt-get install -t jessie-backports linux-headers-4.7.0-0.bpo.1-amd64
apt-get install vim less
apt-get install zip unzip gzip bzip2 p7zip-full

EOF
chmod +x $ROOT_MOUNT_POINT/root/install.sh

chroot $ROOT_MOUNT_POINT /root/install.sh

umount $ROOT_MOUNT_POINT/sys
umount $ROOT_MOUNT_POINT/proc
umount $ROOT_MOUNT_POINT/dev
umount $ROOT_MOUNT_POINT
sync

# Under native system:
# Xorg -configure && mv /root/xorg.conf.new /etc/X11/xorg.conf

