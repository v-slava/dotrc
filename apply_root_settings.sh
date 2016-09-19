#!/usr/bin/env bash

if [ "$(id -u)" != "0" ]; then
	echo "You must be root to apply root settings" 1>&2
	exit 1
fi

set -ex

cp -rfv --preserve=mode $PWD/root_settings/* /

update-grub
locale-gen
if systemctl is-enabled systemd-networkd.service ; then
	systemctl disable systemd-networkd.service
fi
# systemctl status wpa_supplicant
systemctl set-default native.target

# apply vifm settings to vim:
cp /usr/share/vim/syntax/vifm.vim /usr/share/vim/vim74/syntax/
cp /usr/share/vim/addons/plugin/vifm.vim /usr/share/vim/vim74/plugin/

# update fonts cache:
fc-cache -fv
# restart udev:
service udev restart

