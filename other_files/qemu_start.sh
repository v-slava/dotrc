#!/usr/bin/env bash

# Create user for qemu:
# echo -e "qemu_user_password\nqemu_user_password\n\n\n\n\n\ny" | adduser qemu_user --no-create-home
# usermod -a -G audio,video,systemd-journal,bluetooth,netdev,kvm,disk qemu_user

# Qemu network configuration (bridge must be configured first):
# chmod u+s /usr/lib/qemu/qemu-bridge-helper
# cat << EOF > /etc/qemu/bridge.conf
# allow br0
# EOF

# Links:
# Spice windows guest tools:
# http://www.spice-space.org/download/windows/spice-guest-tools/spice-guest-tools-0.100.exe
# Virtio guest drivers:
# http://alt.fedoraproject.org/pub/alt/virtio-win/latest/images/bin/virtio-win-0.1-100.iso

# Fix registry entries in windows in order to avoid BSOD:
# cat << EOF > qemu_windows.reg
# Windows Registry Editor Version 5.00
#
# [HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\services\atapi]
# "Start"=dword:00000000
#
# [HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\services\intelide]
# "Start"=dword:00000000
#
# [HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\services\pciide]
# "Start"=dword:00000000
#
# EOF

set -e

# SHARE_DIR="${SDIR}/share"
CTRL_SOCKET="/tmp/ctrl.sock"
PID_FILE="/tmp/qemu.pid"

MAC="E8:03:9A:3D:60:64"
WIN_HDD="/dev/sda"
GRUB_ISO="/home/volkov/temporary/grub.iso"

export QEMU_AUDIO_DRV=pa

sudo -u qemu_user qemu-system-x86_64 \
	--enable-kvm \
	-name Windows -cpu host -m 4G -smp 8 -balloon virtio \
	-boot d -drive file="${GRUB_ISO}",media=cdrom \
	-drive file="${WIN_HDD}",if=virtio,cache=none,snapshot=off \
	-global isa-fdc.driveA= \
	-usb -usbdevice tablet -vga qxl \
	-spice addr=127.0.0.1,port=5930,disable-ticketing \
	-device virtio-serial-pci -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0 \
	-chardev spicevmc,id=spicechannel0,name=vdagent \
	-daemonize -pidfile "${PID_FILE}" -monitor unix:"${CTRL_SOCKET}",server,nowait \
	-netdev bridge,id=hn0,br=br0 -device virtio-net-pci,netdev=hn0,id=nic1,mac=${MAC} \
	$@

	# -netdev bridge,id=hn0,br=br0 -device virtio-net-pci,netdev=hn0,id=nic1,mac=${MAC} \
	# -net none \
	# -soundhw hda \
	# -drive file=image.iso,media=cdrom \
	# -boot d \
	# -drive file="${WIN_HDD}",if=ide,readonly=on,cache=none,snapshot=off \
	# -nodefaults

# -no-frame
# -ctrl-grab
# -m 2G -smp cores=2,threads=4
# -net nic -net user,hostname=windowsvm
# -net nic,model=virtio,macaddr=${MAC} -net vde
# -virtfs local,path="${SHARE_DIR}",security_model=passthrough,mount_tag=host_share
# -drive file=fat:ro:"${SHARE_DIR}",if=virtio
# -net nic,macaddr=${MAC} -net user,hostname=a-kozhemyach-l2

i3-msg "workspace 8; exec exec x-terminal-emulator -title \"Qemu monitor\" -e sudo -u qemu_user nc -U /tmp/ctrl.sock"
i3-msg "workspace 9; exec exec spicec -t 'Qemu: Windows 7' -h localhost -p 5930"

