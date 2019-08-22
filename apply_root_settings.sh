#!/usr/bin/env bash

if [ -n "$WINDIR" ]; then
    echo -e "This script is not applicable for windows." 1>&2
    read -p "Press enter to continue..."
    exit 1
fi

if [ "$(id -u)" != "0" ]; then
    echo "You must be root to apply root settings" 1>&2
    exit 1
fi

set -ex
cp -rfv --preserve=mode $PWD/root_settings/* /

ROOT_SETTINGS_S="$DOTRC_S/root_settings"
set +ex
if [ -d "$ROOT_SETTINGS_S" ]; then
    set -e
    cd "$ROOT_SETTINGS_S"
    find -type f | while read file ; do
        orig_file="$(echo $file | cut -c 2-)"
        repo_file="$ROOT_SETTINGS_S$orig_file"
        cmd="cat $repo_file >> $orig_file"
        echo "+ $cmd"
        eval $cmd
    done
    cd - 1>/dev/null
fi
set -ex

# update-grub
efibootmgr -b 0000 -B
efibootmgr -c -L "Debian (EFI stub)" -l '/EFI/debian/vmlinuz' -u 'root=/dev/sda2 add_efi_memmap initrd=/EFI/debian/initrd.img acpi_backlight=vendor pci=noaer'

locale-gen
if systemctl is-enabled systemd-networkd.service ; then
	systemctl disable systemd-networkd.service
fi
# systemctl status wpa_supplicant
# systemctl set-default default_system_gui.target

# apply vifm settings to vim:
# cp /usr/share/vim/addons/syntax/vifm.vim /usr/share/vim/vim74/syntax/
# cp /usr/share/vim/addons/plugin/vifm.vim /usr/share/vim/vim74/plugin/

# update fonts cache:
fc-cache -fv
# restart udev:
service udev restart

echo -e "\nDone!"
