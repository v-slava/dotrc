#!/usr/bin/env bash

if [ "$(id -u)" != "0" ]; then
	echo "You must be root to apply root settings" 1>&2
	exit 1
fi

set -ex

cp -rafv $PWD/root_settings/* /

update-grub
locale-gen
systemctl enable systemd-networkd.service
systemctl enable wpa_supplicant
systemctl set-default volkov.target

