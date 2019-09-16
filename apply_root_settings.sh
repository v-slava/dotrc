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

if [ -z "$DOTRC" ]; then
    echo "Error: \$DOTRC is not defined" 1>&2
    exit 1
fi

if [ -z "$DOTRC_S" ]; then
    echo "Error: \$DOTRC_S is not defined" 1>&2
    exit 1
fi

set -e

source $DOTRC/other_files/config_file.sh
list_files()
(
    DIR="$1"
    set -e -o pipefail
    find "$DIR" ! -type d | cut -c $((${#DIR} + 1))-
)
(
    set -e
    list_files $DOTRC/root_settings/
    if [ -d $DOTRC_S/root_settings ]; then
        list_files $DOTRC_S/root_settings/
    fi
) | sort -u | while read FILE ; do
    config_generate -r "$FILE"
done

if $DOTRC/other_files/virtual_box.sh ; then
    rm -f /etc/systemd/system/wpa_supplicant.service
fi

LINUX_ARGS=()
if ! which dmidecode 1>/dev/null 2>&1 ; then
    echo "Missing: apt-get install dmidecode" 1>&2
    exit 1
fi
LAPTOP="$(dmidecode -s system-manufacturer) $(dmidecode -s system-product-name)"
case "${LAPTOP,,}" in
    asus*x541ua*) LINUX_ARGS+=(acpi_backlight=vendor pci=noaer) ;;
esac

    echo "Generating /etc/fstab ..."
    cat << EOF > /etc/fstab
# <file system>        <dir>         <type>    <options>           <dump> <pass>
EOF

if [ -d /sys/firmware/efi ]; then
    LINUX_ARGS+=(root=/dev/sda2 initrd=/EFI/debian/initrd.img)
    LINUX_ARGS+=(add_efi_memmap)
    LINUX_ARGS="${LINUX_ARGS[@]}"
    cat << EOF >> /etc/fstab
/dev/sda1              /boot/efi     vfat      defaults              0      0
/dev/sda2              /             ext4      defaults,noatime      0      1
/dev/sda3              /media/files  ext4      defaults,noatime      0      2
EOF
    set +e -x
    efibootmgr -b 0000 -B
    set -e
    efibootmgr -c -L 'Debian (EFI stub)' -l '/EFI/debian/vmlinuz' -u "$LINUX_ARGS"
else
    cat << EOF >> /etc/fstab
/dev/sda1              /             ext4      defaults,noatime      0      1
/dev/sda2              /media/files  ext4      defaults,noatime      0      2
EOF
    rm -f /etc/initramfs/post-update.d/zz-update-efistab
    rm -f /etc/kernel/postinst.d/zz-update-efistab
    sed -i /etc/default/grub -e \
's|^GRUB_CMDLINE_LINUX_DEFAULT="quiet"$|GRUB_CMDLINE_LINUX_DEFAULT="rw"|g'
    echo "Updating GRUB ..."
    update-grub
fi

    cat << EOF >> /etc/fstab
# /dev/sda5              none          swap      defaults              0      0
# Mount /media/usb:
/dev/sdb1              /media/usb   vfat noauto,noatime,user,flush,rw,exec 0 0
/dev/sdc1              /media/usb   vfat noauto,noatime,user,flush,rw,exec 0 0
EOF

echo "Generating locales ..."
locale-gen 1>/dev/null
if systemctl is-enabled systemd-networkd.service ; then
    systemctl disable systemd-networkd.service 1>/dev/null
fi
# systemctl status wpa_supplicant
# systemctl set-default default_system_gui.target

# apply vifm settings to vim:
# cp /usr/share/vim/addons/syntax/vifm.vim /usr/share/vim/vim74/syntax/
# cp /usr/share/vim/addons/plugin/vifm.vim /usr/share/vim/vim74/plugin/

echo "Updating font cache ..."
fc-cache -f
# restart udev:
service udev restart

if [ -x $DOTRC_S/apply_root_settings.sh ]; then
    $DOTRC_S/apply_root_settings.sh
fi

echo -e "\nDone!"
