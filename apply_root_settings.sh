#!/usr/bin/env bash

if [ "$(id -u)" != "0" ]; then
	echo "You must be root to apply root settings" 1>&2
	exit 1
fi

set -ex

cp -rfv --preserve=mode $PWD/root_settings/* /

update-grub
locale-gen
if ! systemctl is-enabled systemd-networkd.service ; then
	systemctl enable systemd-networkd.service
fi
if ! systemctl is-enabled wpa_supplicant.service ; then
	systemctl enable wpa_supplicant
fi
systemctl set-default volkov.target

