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

LINUX_ARGS="rw"
if ! which dmidecode 1>/dev/null 2>&1 ; then
    echo "Missing: apt-get install dmidecode" 1>&2
    exit 1
fi
LAPTOP="$(dmidecode -s system-manufacturer) $(dmidecode -s system-product-name)"
case "${LAPTOP,,}" in
    asus*x541ua*) LINUX_ARGS="$LINUX_ARGS acpi_backlight=vendor pci=noaer" ;;
esac

echo "Updating grub..."
sed -i /etc/default/grub -e \
"s|^# TODO sed replace: GRUB_CMDLINE_LINUX_DEFAULT=\"some_args\"$|\
GRUB_CMDLINE_LINUX_DEFAULT=\"$LINUX_ARGS\"|g"

update-grub

echo "Generating locales..."
locale-gen 1>/dev/null

echo "Configuring services..."
systemctl-is-enabled() {
    SERVICE="$1"
    [ $(systemctl list-unit-files "$SERVICE" | wc -l) -gt 3 ] \
        && systemctl is-enabled "$SERVICE" 1>/dev/null
}

if systemctl-is-enabled systemd-networkd.service ; then
    systemctl disable systemd-networkd.service 1>/dev/null
fi
if systemctl-is-enabled minidlna.service ; then
    systemctl disable minidlna.service 1>/dev/null
fi
# systemctl set-default default_system_gui.target

# apply vifm settings to vim:
# cp /usr/share/vim/addons/syntax/vifm.vim /usr/share/vim/vim74/syntax/
# cp /usr/share/vim/addons/plugin/vifm.vim /usr/share/vim/vim74/plugin/

echo "Updating font cache ..."
fc-cache -f
# restart udev:
service udev restart

WAIT_FOR_INPUT=/media/files/programs/wait_for_input
gcc -O2 -flto $DOTRC/other_files/wait_for_input.c -o $WAIT_FOR_INPUT
chown slava:input $WAIT_FOR_INPUT
sudo chmod g+s $WAIT_FOR_INPUT

if [ -x $DOTRC_S/apply_root_settings.sh ]; then
    $DOTRC_S/apply_root_settings.sh
fi

echo -e "\nDone!"
