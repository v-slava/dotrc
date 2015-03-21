#!/usr/bin/env bash

# already installed packages:
# apt-get install linux-image-amd64 grub-pc os-prober udev kmod

apt-get upgrade --yes

# In wheezy: iproute2 conflicts with iproute, but ifupdown depends on iproute
# Wheezy-only packages:
# apt-get install iproute ifupdown dhcpcd5 rsyslog sudo uncrustify

# Jessie-only packages:
apt-get install iproute2 vifm

# Install non-gui packages:
apt-get install apt-file apt-rdepends \
	usbutils pciutils util-linux lsof vim less bash-completion \
	apt-utils dialog locales wpasupplicant dbus iputils-ping \
	wireless-tools iptables hostapd dnsmasq traceroute wget \
	man-db manpages manpages-dev manpages-posix manpages-posix-dev info \
	smbclient smbnetfs openssh-client sshfs fuse \
	gcc gcc-doc libc-dev glibc-doc glibc-doc-reference strace ltrace \
	socat psmisc zip unzip bzip2 unrar \
	exuberant-ctags cscope doxygen graphviz pv htop colordiff \
	tree git make patch dos2unix bc file dtach bsdutils android-tools-adb \

# xdg-user-dirs cifs-utils

# Build and install vifm (wheezy):
# apt-get install libncursesw5-dev
# make -f ./other_files/Makefile_vifm
# make -f ./other_files/Makefile_vifm clean
# Jessie:
apt-get install vifm

# Install audio player (no X required):
apt-get install alsa-utils \
	alsaplayer-daemon alsaplayer-common

apt-file update

# Install xorg:
apt-get install xorg xserver-xorg-video-intel \
	xserver-xorg-input-evdev xinit rxvt-unicode-256color

# Install window manager, status bar, screen locker, keyboard layout
# indicator/switcher and fonts:
# Wheezy:
# I3_WM=i3-wm=4.8-1~bpo70+1
# Jessie:
I3_WM=i3-wm
apt-get install $I3_WM i3status i3lock \
	fbxkb fonts-inconsolata
# bspwm2 - similar to i3, unstable, configuration from scratch

# Install X helper programs (we need xclip because of uzbl):
apt-get install wmctrl xclip scrot zenity

# Install vim instance, which is able to access X clipboard:
apt-get install vim-gtk

# Install email client:
apt-get install claws-mail
# Wheezy:
# apt-get install claws-mail-trayicon
# Jessie:
apt-get install claws-mail-multi-notifier
# icedove (thunderbird)

# Install offline vocabulary:
# apt-get install goldendict
# apt-get install sdcv

# Install pulseaudio:
apt-get install pulseaudio pulseaudio-utils pavucontrol

# Install video player:
apt-get install mplayer smplayer
# Install alternative video player:
# apt-get install vlc

# Install images viewer:
apt-get install gliv

# Install images editor:
# apt-get install gimp # kolourpaint4

# Install torrent client:
apt-get install transmission-gtk

# Install office suite:
apt-get install libreoffice # libreoffice-pdfimport
# Install alternative office suite:
# apt-get install abiword gnumeric

# Install pdf viewer:
apt-get install apvlv
# mupdf, zathura

# Build and install vimb:
apt-get install libwebkitgtk-3.0-dev
make -f ./other_files/Makefile_vimb
make -f ./other_files/Makefile_vimb clean
# apt-get purge libwebkitgtk-3.0-dev
# apt-get autoremove --purge

# Install alternative browsers:
# apt-get install iceweasel iceweasel-l10n-ru # flashplugin-nonfree

# Wheezy:
# CHROMIUM_LANG=chromium-l10n
# Jessie:
CHROMIUM_LANG=chromium-browser-l10n
apt-get install chromium $CHROMIUM_LANG # pepperflashplugin-nonfree


# dmenu:
# apt-get install suckless-tools # for dmenu
# Install build dependencies and build dmenu from sources with ttf support:
apt-get install libx11-dev libxinerama-dev libxft-dev
make -f ./other_files/Makefile_dmenu
make -f ./other_files/Makefile_dmenu clean
apt-get purge libx11-dev libxinerama-dev libxft-dev
apt-get autoremove --purge

dpkg --add-architecture i386
apt-get update
# Download skype.deb via browser
# dpkg -i skype.deb # install skype
# apt-get -f install # fix missing dependencies
# apt-get install dbus dbus-x11

