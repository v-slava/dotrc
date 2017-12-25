#!/usr/bin/env bash

# already installed packages: linux-image-amd64 grub-pc os-prober

set -ex

apt-get upgrade --yes

# Install non-gui packages:
apt-get install udev kmod sudo usbutils pciutils util-linux lsof \
	vim vifm less bash-completion python youtube-dl cclive dhex \
	apt-file apt-rdepends apt-utils dialog locales isc-dhcp-client \
	wpasupplicant iputils-ping iproute2 net-tools wireless-tools iptables traceroute wget \
	man-db manpages manpages-dev manpages-posix manpages-posix-dev info \
	openssh-client sshfs jmtpfs fuse silversearcher-ag kbd \
	gcc gcc-doc libc-dev glibc-doc glibc-doc-reference strace ltrace bear \
	g++ clang-tidy-3.8 gdb gdb-doc gdbserver gdb-multiarch \
	zip unzip gzip xz-utils bzip2 p7zip-full cpio unrar \
	sox libsox-fmt-mp3 libav-tools \
	exuberant-ctags cscope doxygen graphviz pv htop colordiff socat psmisc \
	tree git make patch dos2unix file dtach bsdutils android-tools-adb \
	expect lame ntpdate ntfs-3g fuseiso9660 netcat-openbsd keepass2 qalculate \

apt-file update

# For Asus F541U (bluetooth) (not stable WI-FI, see dmesg -w):
# apt-get install -t jessim-backports firmware-atheros
# For Asus F541U:
apt-get install firmware-realtek firmware-misc-nonfree

# Install xorg:
apt-get install xorg xserver-xorg-video-intel xserver-xorg-input-evdev \
	xserver-xorg-input-synaptics xinit rxvt-unicode-256color

# Install window manager, status bar, screen locker, keyboard layout
# indicator:
apt-get install -t jessie-backports i3-wm libanyevent-i3-perl i3status i3lock fbxkb

# Install network manager:
apt-get install network-manager-gnome gnome-keyring notification-daemon

# Install PulseAudio:
apt-get install -t jessie-backports pulseaudio pulseaudio-module-bluetooth pulseaudio-utils
apt-get install bluez pavucontrol

# Install audio player:
apt-get install alsa-utils alsaplayer-daemon alsaplayer-common

# Install qemu:
apt-get install qemu-system-x86 qemu-kvm spice-client

# Install X helper programs:
apt-get install wmctrl xdotool xclip xinput xbacklight scrot zenity xcape xprintidle uim-gtk2.0
# keynav

# Install vim instance, which is able to access X clipboard:
apt-get install vim-gtk

# Install email client (thunderbird):
apt-get install icedove

# Install video player:
apt-get install mplayer2 smplayer

# Install images viewer:
apt-get install gliv

# Install images editor:
apt-get install gimp

# Install vocabulary:
apt-get install goldendict

# Install torrent client:
apt-get install transmission-gtk

# Install office suite:
apt-get install libreoffice

# Install pdf viewer:
apt-get install zathura

# Install whatever is necessary for spacemacs:
apt-get install emacs python-jedi python-setuptools clang-format-3.8

# Install alternative browsers:
# apt-get install iceweasel iceweasel-l10n-ru
apt-get install chromium chromium-l10n pepperflashplugin-nonfree uzbl
# On wheezy: chromium-browser-l10n

# dmenu:
make -f ~/os_settings/other_files/Makefile_dmenu.mk

# Coreutils viewer:
apt-get install libncurses5-dev pkg-config
make -f ~/os_settings/other_files/Makefile_coreutils_viewer.mk

# vifm:
# make -f ~/os_settings/other_files/Makefile_vifm.mk

# dpkg --add-architecture i386
# apt-get update

# Download skype.deb via browser
# dpkg -i skype.deb # install skype
# apt-get -f install # fix missing dependencies

# Build and install vimb:
# apt-get install libwebkitgtk-dev libwebkitgtk-1.0-0 flashplugin-nonfree
# make -f ~/os_settings/other_files/Makefile_vimb

# Other/old packages:
# hostapd dnsmasq cifs-utils smbclient smbnetfs goldendict sdcv dbus dbus-x11
# libreoffice-pdfimport abiword gnumeric mupdf suckless-tools # (dmenu)
# kolourpaint4 vlc apvlv fonts-inconsolata
# claws-mail-multi-notifier claws-mail # On wheezy: claws-mail-trayicon

