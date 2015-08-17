#!/usr/bin/env bash

# already installed packages: linux-image-amd64 grub-pc os-prober

apt-get upgrade --yes

# Install non-gui packages:
apt-get install udev kmod sudo usbutils pciutils util-linux lsof \
	vim vifm less bash-completion \
	apt-file apt-rdepends apt-utils dialog locales \
	wpasupplicant iputils-ping iproute2 wireless-tools iptables traceroute wget \
	man-db manpages manpages-dev manpages-posix manpages-posix-dev info \
	openssh-client sshfs fuse \
	gcc gcc-doc libc-dev glibc-doc glibc-doc-reference strace ltrace \
	zip unzip gzip bzip2 p7zip-full cpio unrar \
	alsa-utils alsaplayer-daemon alsaplayer-common \
	bluez pulseaudio pulseaudio-utils pulseaudio-module-bluetooth \
	exuberant-ctags cscope doxygen graphviz pv htop colordiff socat psmisc \
	tree git make patch dos2unix bc file dtach bsdutils android-tools-adb \
	expect mpg321 ntpdate ntfs-3g fuseiso9660 qemu-system-x86 qemu-kvm \
	spice-client netcat-openbsd

apt-file update

# Install xorg:
apt-get install xorg xserver-xorg-video-intel \
	xserver-xorg-input-evdev xinit rxvt-unicode-256color

# Install window manager, status bar, screen locker, keyboard layout
# indicator and fonts:
apt-get install i3-wm i3status i3lock \
	fbxkb fonts-inconsolata network-manager-gnome

# Install X helper programs:
apt-get install wmctrl xclip scrot zenity keynav

# Install vim instance, which is able to access X clipboard:
apt-get install vim-gtk

# Install email client (thunderbird):
apt-get install icedove

# Install PulseAudio tools:
apt-get install pulseaudio-utils pavucontrol

# Install video player:
apt-get install mplayer smplayer

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

# Build and install vimb:
apt-get install libwebkitgtk-dev libwebkitgtk-1.0-0 flashplugin-nonfree
make -f ./other_files/Makefile_vimb
make -f ./other_files/Makefile_vimb clean
apt-get purge libwebkitgtk-dev
apt-get autoremove --purge

# Install alternative browsers:
# apt-get install iceweasel iceweasel-l10n-ru
apt-get install chromium chromium-l10n pepperflashplugin-nonfree
# On wheezy: chromium-browser-l10n

# dmenu:
apt-get install libx11-dev libxinerama-dev libxft-dev libxmu-dev
make -f ./other_files/Makefile_dmenu
make -f ./other_files/Makefile_dmenu clean
apt-get purge libx11-dev libxinerama-dev libxft-dev libxmu-dev
apt-get autoremove --purge

# Coreutils viewer:
apt-get install libncurses5-dev
make -f ./other_files/Makefile_coreutils_viewer
apt-get purge libncurses5-dev

dpkg --add-architecture i386
apt-get update
# Download skype.deb via browser
# dpkg -i skype.deb # install skype
# apt-get -f install # fix missing dependencies

# Other/old packages:
# hostapd dnsmasq cifs-utils smbclient smbnetfs goldendict sdcv dbus dbus-x11
# libreoffice-pdfimport abiword gnumeric mupdf zathura suckless-tools # (dmenu)
# gliv kolourpaint4 vlc apvlv
# claws-mail-multi-notifier claws-mail # On wheezy: claws-mail-trayicon

# On wheezy: iproute2 conflicts with iproute, but ifupdown depends on iproute
# Wheezy-only packages: iproute ifupdown dhcpcd5 rsyslog uncrustify

