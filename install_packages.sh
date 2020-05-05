#!/usr/bin/env bash

# Install debian:
# mkfs -t ext4 -L root_partition /dev/sdX2
# mount /dev/sdX2 rootfs_dir
# debootstrap --arch=amd64 --variant=minbase bullseye rootfs_dir

# copy /etc/apt/apt.conf, create /etc/fstab
# mount -B /proc rootfs_dir/proc
# mount -B /sys rootfs_dir/sys
# mount -B /dev rootfs_dir/dev
# mount -B /dev/pts rootfs_dir/dev/pts
# chroot rootfs_dir
# passwd

# apt-get install linux-image-amd64 systemd systemd-sysv
# apt-get install efibootmgr
# apply postinst for kernel and initramfs
# alternative: apt-get install grub-pc os-prober

# apt-get install network-manager

# exit && umount -R rootfs_dir && sync && reboot

set -ex

apt-get upgrade --yes

# Install non-gui packages:
apt-get install udev kmod sudo usbutils pciutils util-linux lsof \
    vbindiff vifm ripgrep progress fzf less bash-completion python xxd \
    apt-file apt-rdepends apt-utils dialog locales \
    wpasupplicant iputils-ping iproute2 net-tools wireless-tools iptables \
    isc-dhcp-client traceroute wget \
    man-db manpages manpages-dev manpages-posix manpages-posix-dev info \
    gcc-doc libc-dev glibc-doc glibc-doc-reference \
    gcc g++ cmake build-essential clang clang-format clang-tidy clang-tools \
    gdb gdb-doc gdbserver gdb-multiarch uftrace strace ltrace graphviz python3 \
    linux-base linux-perf ripgrep bvi \
    rtags exuberant-ctags cscope doxygen git git-email make patch dos2unix \
    zip unzip gzip xz-utils bzip2 p7zip-full cpio unrar pv htop \
    colordiff socat psmisc ffmpeg tree file bsdutils openssh-client \
    ntpdate fuseiso9660 netcat-openbsd keepass2 \

# sox libsox-fmt-mp3 android-tools-adb lame fuse kbd sshfs qalculate ntfs-3g
# libclang-dev llvm vim dhex
# Install my (patched) version for:
# bear

apt-file update

apt-get install go-mtpfs
# jmtpfs

# For Asus F541U (bluetooth) (not stable WI-FI, see dmesg -w):
# apt-get install firmware-atheros
# For Asus F541U:
apt-get install firmware-realtek firmware-misc-nonfree intel-microcode

# Install GUI:
# apt-get install sway swaylock kitty wofi
# dunst -> mako (desktop notifications)
# dmenu -> bemenu
# xclip, xsel -> wl-clipboard
# scrot -> slurp, grim (screenshots)
# Commands:
# sway
# WAYLAND_DISPLAY=wayland-0 kitty
# KITTY_ENABLE_WAYLAND=1 kitty # no effect
# export EGL_LOG_LEVEL=debug
# export LIBGL_DEBUG=verbose
# export MESA_DEBUG=1
# See also egl_version.c in $DOTRC/other_files/my_samples/single_sources/egl_version.c

# Install xorg (use xserver-xorg-video-vmware for virtualbox):
apt-get install xorg xserver-xorg-video-intel xserver-xorg-input-libinput \
    xinit rxvt-unicode-256color rxvt-unicode libpam-systemd
# Alternative for evdev and synaptics: xserver-xorg-input-libinput

# Install window manager, status bar, screen locker, keyboard layout
# indicator:
apt-get install i3-wm libanyevent-i3-perl i3status i3lock fbxkb

# Install network manager:
apt-get install network-manager-gnome gnome-keyring notification-daemon

# Install PulseAudio:
apt-get install pulseaudio pulseaudio-module-bluetooth pulseaudio-utils
apt-get install bluez pavucontrol
# apt-get install bluez-tools

# Install audio player:
# apt-get install alsa-utils alsaplayer-daemon alsaplayer-common
apt-get install xmms2-client-nycli xmms2-plugin-mpg123 xmms2-plugin-pulse
apt-get install id3v2 alsa-utils # for volume control (amixer)

# Install qemu:
# apt-get install qemu-system-x86 qemu-kvm spice-client

# Install X helper programs:
apt-get install wmctrl xdotool xsel xinput xbacklight scrot zenity xcape xprintidle uim-gtk2.0
# keynav

# Destop notifications (use notify-send to send a message):
apt-get install libnotify-bin dunst
# notify-osd # heavy-weight, has issues with i3wm

# Install vim instance, which is able to access X clipboard:
apt-get install neovim
apt-get install --install-recommends python3-pip
pip3 install --user neovim youtube_dl psutil

# Install email client:
apt-get install evolution
# thunderbird icedove msmtp mutt mmh
# For microsoft exchange server: evolution-ews

# Install video player:
apt-get install smplayer

# Install images viewer:
apt-get install gliv

# Install images editor:
# apt-get install gimp
apt-get install kolourpaint

# Install vocabulary:
apt-get install goldendict

# Install torrent client:
apt-get install transmission-gtk

# Install office suite:
apt-get install libreoffice

# Install pdf viewer:
apt-get install zathura
# editable pdf support: evince

# Install alternative browsers:
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
dpkg -i google-chrome-stable_current_amd64.deb
# apt-get install iceweasel iceweasel-l10n-ru
# apt-get install chromium
# chromium-l10n pepperflashplugin-nonfree uzbl

# dmenu:
make -f $DOTRC/other_files/build_or_install_scripts/dmenu/Makefile_dmenu.mk

# fzy:
# apt-get install fzy
# wget http://ftp.de.debian.org/debian/pool/main/f/fzy/fzy_1.0-1_amd64.deb

# Install also: Viber, Skype, Telegram, Teamviewer

# Coreutils viewer:
# apt-get install libncurses5-dev pkg-config
# make -f $DOTRC/other_files/Makefile_coreutils_viewer.mk

# vifm:
# make -f $DOTRC/other_files/Makefile_vifm.mk

# dpkg --add-architecture i386
# apt-get update

# Download skype.deb via browser
# dpkg -i skype.deb # install skype
# apt-get -f install # fix missing dependencies

# Build and install vimb:
# apt-get install libwebkitgtk-dev libwebkitgtk-1.0-0 flashplugin-nonfree
# make -f $DOTRC/other_files/Makefile_vimb

# Other/old packages:
# hostapd dnsmasq cifs-utils smbclient smbnetfs goldendict sdcv dbus dbus-x11
# libreoffice-pdfimport abiword gnumeric mupdf suckless-tools # (dmenu)
# kolourpaint4 vlc apvlv fonts-inconsolata
# claws-mail-multi-notifier claws-mail # On wheezy: claws-mail-trayicon

