#!/usr/bin/env bash

if [ "$(id -u)" != "0" ]; then
	echo "You must be root to apply root settings" 1>&2
	exit 1
fi

set -ex

cp -rfv --preserve=mode $PWD/root_settings/* /

ROOT_SETTINGS_S='/media/files/workspace/dotrc_s/root_settings'
if [ -d "$ROOT_SETTINGS_S" ]; then
    set +x
    cd "$ROOT_SETTINGS_S"
    find -type f | while read file ; do
        orig_file="$(echo $file | cut -c 2-)"
        repo_file="$ROOT_SETTINGS_S$orig_file"
        cmd="cat $repo_file >> $orig_file"
        echo $cmd
        eval $cmd
    done
    cd - 1>/dev/null
    set -x
fi

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

