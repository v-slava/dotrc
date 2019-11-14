#!/bin/sh

cd "$(dirname "$0")" || exit 1
SDIR="${PWD}"
SHARE_DIR="${SDIR}/share"
CTRL_SOCKET="${SDIR}/ctrl.sock"
PIDFILE="${SDIR}/qemu.pid"

TMP_SNAPSHOT=off

QEMU="qemu-system-x86_64"
QEMU="${QEMU} -enable-kvm"
#QEMU="sudo ${QEMU}"
#sudo chmod u+s /usr/lib/qemu/qemu-bridge-helper

MAC=""
WINDEV="/dev/sda3"
#DISK="disk.qcow2"
DISK="disk-s1.qcow2"

export QEMU_AUDIO_DRV=pa

exec ${QEMU} \
	-name Windows \
	-cpu host \
	-m 2G -smp 4 \
	-balloon virtio \
	-snapshot \
	-drive file="${DISK}",if=virtio,snapshot=${TMP_SNAPSHOT} \
	-drive file="${WINDEV}",if=virtio,cache=none,snapshot=off \
	-netdev bridge,id=hn0,br=br0 -device virtio-net-pci,netdev=hn0,id=nic1,mac=${MAC} \
	-usb -usbdevice tablet \
	-soundhw hda \
	-vga qxl \
	-spice addr=127.0.0.1,port=5930,disable-ticketing \
	-device virtio-serial-pci -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0 \
	-chardev spicevmc,id=spicechannel0,name=vdagent \
	-daemonize -pidfile "${PIDFILE}" -monitor unix:"${CTRL_SOCKET}",server,nowait \
	$@

# -runas your_user_name
# -no-frame
# -ctrl-grab
# -m 2G -smp cores=2,threads=4
# -boot d -drive file=install.iso,media=cdrom -drive file=virtio.iso,media=cdrom
# -net nic -net user,hostname=windowsvm
# -net nic,model=virtio,macaddr=${MAC} -net vde
# -virtfs local,path="${SHARE_DIR}",security_model=passthrough,mount_tag=host_share
# -drive file=fat:ro:"${SHARE_DIR}",if=virtio

# -net nic,macaddr=${MAC} -net user,hostname=a-kozhemyach-l2
# -netdev bridge,id=hn0,br=br0 -device virtio-net-pci,netdev=hn0,id=nic1,mac=${MAC}
# -netdev tap,id=net0,script=${PWD}/qemu-ifup,downscript=no,helper=${PWD}/qemu-bridge-helper -device virtio-net-pci,netdev=net0,mac=${MAC}
