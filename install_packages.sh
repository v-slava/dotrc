#!/usr/bin/env bash

# Install debian:
# mkfs -t vfat -F 32 -n EFI /dev/sdX1
# mkfs -t ext4 -L root_partition /dev/sdX2
# mkfs -t ext4 -m 0 -L files_partition /dev/sdX3
# mount /dev/sdX2 rootfs_dir
# debootstrap --arch=amd64 --variant=minbase bullseye rootfs_dir

# copy /etc/apt/apt.conf, create /etc/fstab
# mount -B /proc rootfs_dir/proc
# mount -B /sys rootfs_dir/sys
# mount -B /dev rootfs_dir/dev
# mount -B /dev/pts rootfs_dir/dev/pts
# chroot rootfs_dir
# passwd
# mount /dev/sda1 /boot/efi
# mkdir /media/files

# apt-get install linux-image-amd64 systemd systemd-sysv
#
# alternative #1 (EFI stub, PC firmware may ignore kernel arguments):
# apt-get install efibootmgr
# apply postinst for kernel and initramfs
#
# alternative #2 (BIOS grub2, obsolete):
# apt-get install grub-pc os-prober
#
# alternative #2 (EFI grub2, recommended):
# mount /dev/sda1 /boot/efi # 512M
# mkdir -p /boot/efi/EFI/Debian
# apt-get install grub-efi-amd64 os-prober
#
# For grub2:
# sed -i /etc/default/grub -e \
# 's|^GRUB_CMDLINE_LINUX_DEFAULT="quiet"$|GRUB_CMDLINE_LINUX_DEFAULT="rw"|g'
# update-grub

# apt-get install network-manager

# exit && umount -R rootfs_dir && sync && reboot

set -ex

apt-get upgrade --yes

# Install non-gui packages:
apt-get install udev kmod sudo usbutils pciutils util-linux lsof \
    vbindiff vifm ripgrep progress fzf less bash-completion xxd \
    apt-file apt-rdepends apt-utils dialog locales \
    wpasupplicant iputils-ping iproute2 net-tools wireless-tools iptables \
    isc-dhcp-client traceroute wget at kbd \
    man-db manpages manpages-dev manpages-posix manpages-posix-dev info \
    gcc-doc libc-dev glibc-doc glibc-doc-reference \
    gcc g++ cmake build-essential jq \
    gdb gdb-doc gdbserver gdb-multiarch strace ltrace graphviz python3 \
    linux-base linux-perf bvi git git-email make patch dos2unix \
    zip unzip gzip lzip xz-utils bzip2 p7zip-full cpio unrar zstd pv htop \
    colordiff socat psmisc ffmpeg tree file bsdutils openssh-client \
    ntpdate fuseiso9660 netcat-openbsd \

# apt-get install rsyslog
# apt-get install clang clang-format clang-tidy clang-tools llvm libclang-dev
# apt-get install uftrace

exit

# exuberant-ctags cscope doxygen
# sox libsox-fmt-mp3 android-tools-adb lame fuse sshfs qalculate ntfs-3g
# vim dhex
# Install my (patched) version for:
# bear rtags

apt-file update

# To mount android file system:
# apt-get install go-mtpfs
# another option: jmtpfs

# To mount ios file system:
# apt-get install ifuse usbmuxd

# For Asus F541U (bluetooth) (not stable WI-FI, see dmesg -w):
# apt-get install firmware-atheros
# For Asus F541U:
sudo apt-get install firmware-realtek firmware-misc-nonfree intel-microcode

# For Dell Latitude 5401:
sudo apt-get install firmware-misc-nonfree wireless-regdb firmware-iwlwifi

# Install GUI:
# Install sway:
sudo apt-get install sway # way-cooler libpam-systemd at-spi2-core

# Install terminal emulator:
sudo apt-get install cargo libxcb-shape0-dev libxcb-xfixes0-dev
cargo install alacritty
# sudo apt-get install kitty # foot

# Install helper programs:
sudo apt-get install i3status swaylock wl-clipboard qtwayland5 xwayland
# i3bar -> waybar # or by default: sway-bar
sudo apt-get install ydotool # wtype # xdotool
sudo apt-get install slurp && pip3 install --user swayinfo # xprop

# Install network manager:
sudo apt-get install network-manager-gnome gnome-keyring notification-daemon
# For sway run nm-connection-editor, nm-applet doesn't work. See also nmtui.

# Install PulseAudio:
sudo apt-get install pulseaudio pulseaudio-module-bluetooth pulseaudio-utils
sudo apt-get install bluez pavucontrol
# apt-get install bluez-tools

# Install audio player:
# apt-get install alsa-utils alsaplayer-daemon alsaplayer-common
sudo apt-get install xmms2-client-nycli xmms2-plugin-mpg123 xmms2-plugin-pulse
sudo apt-get install id3v2 alsa-utils # for volume control (amixer)

# Install qemu:
# apt-get install qemu-system-x86 qemu-kvm spice-client

# Install X helper programs:
sudo apt-get install zenity wl-clipboard # xclip, xsel
# apt-get install wmctrl xinput xbacklight xcape xprintidle uim-gtk2.0 keynav
sudo apt-get install grim # scrot (screenshots)

# Destop notifications (use notify-send to send a message):
sudo apt-get install libnotify-bin mako-notifier # dunst (for X11)
# notify-osd # heavy-weight, has issues with i3wm

# Install vim instance, which is able to access X clipboard:
sudo apt-get install neovim python3-neovim
sudo apt-get install python3-psutil # needed for $START_S in sway/i3
sudo apt-get install --install-recommends python3-pip
sudo apt-get install libcairo2-dev # needed for $DOTRC/other_files/update_system.sh
pip3 install --user youtube_dl # neovim psutil

# Install password manager:
# apt-get install keepass2
sudo apt-get install keepassxc
# See also: https://www.passwordstore.org/
# Current database version: kdbx 3.0, Cipher: AES 256-bit,
# KDF: AES (6000 rounds)

# Install email client:
sudo apt-get install evolution
# thunderbird icedove msmtp mutt mmh
# For microsoft exchange server: evolution-ews

# Install video player:
sudo apt-get install smplayer

# Install images viewers:
sudo apt-get install gwenview imv # gliv

# Install images editor:
# apt-get install gimp
sudo apt-get install kolourpaint

# Install vocabulary:
sudo apt-get install goldendict

# Install torrent client:
sudo apt-get install transmission-gtk

# Install office suite:
sudo apt-get install libreoffice

# Install pdf viewer:
sudo apt-get install zathura
# editable pdf support: evince

# Install alternative browsers:
# wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
# dpkg -i google-chrome-stable_current_amd64.deb
# apt-get install iceweasel iceweasel-l10n-ru
# apt-get install chromium
# chromium-l10n pepperflashplugin-nonfree uzbl

# dmenu for i3:
# make -f $DOTRC/other_files/build_or_install_scripts/dmenu/Makefile_dmenu.mk
# dmenu for sway:
sudo apt-get install dmenu # suckless-tools # bemenu # wofi

# Install rdp server:
# sudo apt-get install xrdp xorgxrdp

# Install rdp client:
sudo apt-get install remmina remmina-plugin-rdp

# Install screen sharing server:
# sudo apt-get install x11vnc

# Install screen sharing client:
sudo apt-get install gvncviewer
# apt-cache search vncviewer

# Install media sharing server for smart TV (DLNA):
sudo apt-get install minidlna

# Record video from screen during lection / meeting / ... :
# sudo apt-get install simplescreenrecorder

# Display keypresses (might be usefull for screen recording):
# sudo apt-get install screenkey

# fzy:
# apt-get install fzy
# wget http://ftp.de.debian.org/debian/pool/main/f/fzy/fzy_1.0-1_amd64.deb

# sudo apt-get -install telegram-cli
# Install also: Viber, Skype, Telegram, Teamviewer
# For teamviewer see issue with login manager/display manager/session:
# https://community.teamviewer.com/t5/Linux-EN/TeamViewer-13-amp-debian-9-3-NO-GUI/td-p/29138
# sudo apt-get install lightdm && sudo systemctl disable lightdm.service
# To run teamviewer:
# 1) Start lightdm manually from my GUI session: systemctl start lightdm.service
# 2) sudo chvt 7 # login to lightdm, spawn a terminal emulator
# 3) DISPLAY=:0 teamviewer # by default DISPLAY would be :1
# 4) sudo chvt 1 # go back to original GUI session
# 5) When done: killall TeamViewer && sudo systemctl stop lightdm.service
# Implicit viber dependencies:
# apt-get install libxcb-icccm4 libxcb-image0 libxcb-keysyms1 libxcb-randr0 \
#     libxcb-render-util0 libxcb-xkb1 libxkbcommon-x11-0
# apt-get -f install # fix missing dependencies

# Use google-chrome for zoom, microsoft teams
# Note: zoom audio and video is lagging in google-chrome...

# Other/old packages:
# hostapd dnsmasq cifs-utils smbclient smbnetfs goldendict sdcv dbus dbus-x11
# libreoffice-pdfimport abiword gnumeric mupdf suckless-tools # (dmenu)
# kolourpaint4 vlc apvlv fonts-inconsolata
# claws-mail-multi-notifier claws-mail # On wheezy: claws-mail-trayicon
