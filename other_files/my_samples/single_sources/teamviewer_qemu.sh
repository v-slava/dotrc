#!/bin/bash

# NO_GUI=true
FROM_SCRATCH=true

TEAMVIEWER=/media/files/programs/teamviewer
FUSE_MOUNT=$TEAMVIEWER/mnt

ROOTFS_FILE=$TEAMVIEWER/rootfs.ext4
KERNEL=$TEAMVIEWER/kernel
INITRD=$TEAMVIEWER/initrd
PID_FILE=$TEAMVIEWER/qemu_pid

set -e

if ! dpkg -l | grep -q '^ii  fuseext2 ' ; then
    echo "+ sudo apt-get install fuseext2"
    sudo apt-get install fuseext2
fi

mkdir -p $TEAMVIEWER
if [ ! -f $ROOTFS_FILE ]; then
    echo "Creating rootfs for launching teamviewer in guest linux OS).."
    TMP_DIR=/tmp/qemu_teamviewer
    TMP_ROOTFS_DIR=$TMP_DIR/rootfs
    TMP_ROOTFS_FILE=$TMP_DIR/rootfs.ext4
    if mount | grep -q $TMP_ROOTFS_FILE ; then
        sudo umount $TMP_ROOTFS_FILE
    fi
    if [ -n "$FROM_SCRATCH" ]; then
        rm -rf $TMP_DIR
        mkdir $TMP_DIR
        cd $TMP_DIR
        mkdir $TMP_ROOTFS_DIR
        fallocate -l $((10 * 1024 * 1024 * 1024)) $TMP_ROOTFS_FILE
        echo '+ wget https://download.teamviewer.com/download/linux/teamviewer_amd64.deb'
        wget https://download.teamviewer.com/download/linux/teamviewer_amd64.deb
        echo "+ mkfs -q -t ext4 -L root_partition $TMP_ROOTFS_FILE"
        mkfs -q -t ext4 -L root_partition $TMP_ROOTFS_FILE
    fi
    echo "+ sudo mount $TMP_ROOTFS_FILE $TMP_ROOTFS_DIR"
    sudo mount $TMP_ROOTFS_FILE $TMP_ROOTFS_DIR
    if [ -n "$FROM_SCRATCH" ]; then
        echo "+ sudo debootstrap --arch=amd64 --variant=minbase bullseye $TMP_ROOTFS_DIR"
        sudo debootstrap --arch=amd64 --variant=minbase bullseye $TMP_ROOTFS_DIR
    fi
    sudo cp /etc/apt/apt.conf $TMP_ROOTFS_DIR/etc/apt/
    sudo cp /etc/default/locale $TMP_ROOTFS_DIR/etc/default/
    sudo cp /etc/locale.gen $TMP_ROOTFS_DIR/etc/
    sudo cp $TMP_DIR/teamviewer_amd64.deb $TMP_ROOTFS_DIR/tmp/
    cat << EOF | sudo tee $TMP_ROOTFS_DIR/etc/sddm.conf
[Autologin]
User=tvuser
Session=lxqt.desktop
EOF
    cat << EOF | sudo chroot $TMP_ROOTFS_DIR
set -e
apt-get update
apt-get upgrade
apt-get install -y dialog locales
apt-get install -y linux-image-amd64 systemd systemd-sysv
apt-get install -y udev kmod sudo neovim vifm tree file less psmisc
DEBIAN_FRONTEND=noninteractive apt-get install -y keyboard-configuration
apt-get install --install-recommends -y lxqt
apt-get install -y network-manager
# For clipboard sharing:
apt-get install spice-vdagent

echo root:root | chpasswd
if ! grep -q tvuser /etc/passwd ; then
    useradd -m -s /bin/bash tvuser
    usermod -a -G sudo,audio,video,systemd-journal,plugdev tvuser
    dpkg -i /tmp/teamviewer_amd64.deb || apt-get -f install -y
    # teamviewer license accept # fails
fi
echo 'tvuser:tvuser' | chpasswd
EOF

    mkdir -p $TMP_ROOTFS_DIR/home/tvuser/.config/autostart
    cat << EOF > $TMP_ROOTFS_DIR/home/tvuser/.config/autostart/lxqt-config-monitor-autostart.desktop
[Desktop Entry]
Comment=Autostart monitor settings for LXQt-config-monitor
Exec=lxqt-config-monitor -l
Name=lxqt-config-monitor-autostart
OnlyShowIn=LXQt
Type=Application
Version=1.0
EOF

    mkdir -p $TMP_ROOTFS_DIR/home/tvuser/.config/lxqt
    cat << EOF > $TMP_ROOTFS_DIR/home/tvuser/.config/lxqt/lxqt-config-monitor.conf
[General]
__userfile__=true

[SavedConfigs]
SavedSettings\1\date=2021-06-21T13:41:07
SavedSettings\1\name=Mon Jun 21 13:41:07 2021
SavedSettings\1\settings\1\connected=true
SavedSettings\1\settings\1\currentMode=81
SavedSettings\1\settings\1\currentModeHeight=1080
SavedSettings\1\settings\1\currentModeRate=@Variant(\0\0\0\x87\x42p\0\0)
SavedSettings\1\settings\1\currentModeWidth=1920
SavedSettings\1\settings\1\enabled=true
SavedSettings\1\settings\1\hash=
SavedSettings\1\settings\1\name=Virtual-1
SavedSettings\1\settings\1\primary=true
SavedSettings\1\settings\1\rotation=1
SavedSettings\1\settings\1\xPos=0
SavedSettings\1\settings\1\yPos=0
SavedSettings\1\settings\2\connected=true
SavedSettings\1\settings\2\currentMode=81
SavedSettings\1\settings\2\currentModeHeight=1080
SavedSettings\1\settings\2\currentModeRate=@Variant(\0\0\0\x87\x42p\0\0)
SavedSettings\1\settings\2\currentModeWidth=1920
SavedSettings\1\settings\2\enabled=false
SavedSettings\1\settings\2\hash=
SavedSettings\1\settings\2\name=Virtual-2
SavedSettings\1\settings\2\primary=false
SavedSettings\1\settings\2\rotation=1
SavedSettings\1\settings\2\xPos=1599
SavedSettings\1\settings\2\yPos=0
SavedSettings\1\settings\3\connected=false
SavedSettings\1\settings\3\hash=
SavedSettings\1\settings\3\name=Virtual-3
SavedSettings\1\settings\4\connected=false
SavedSettings\1\settings\4\hash=
SavedSettings\1\settings\4\name=Virtual-4
SavedSettings\1\settings\size=4
SavedSettings\size=1

[currentConfig]
settings\1\connected=true
settings\1\currentMode=81
settings\1\currentModeHeight=1080
settings\1\currentModeRate=@Variant(\0\0\0\x87\x42p\0\0)
settings\1\currentModeWidth=1920
settings\1\enabled=true
settings\1\hash=
settings\1\name=Virtual-1
settings\1\primary=true
settings\1\rotation=1
settings\1\xPos=0
settings\1\yPos=0
settings\2\connected=true
settings\2\currentMode=81
settings\2\currentModeHeight=1080
settings\2\currentModeRate=@Variant(\0\0\0\x87\x42p\0\0)
settings\2\currentModeWidth=1920
settings\2\enabled=false
settings\2\hash=
settings\2\name=Virtual-2
settings\2\primary=false
settings\2\rotation=1
settings\2\xPos=1599
settings\2\yPos=0
settings\3\connected=false
settings\3\hash=
settings\3\name=Virtual-3
settings\4\connected=false
settings\4\hash=
settings\4\name=Virtual-4
settings\size=4
EOF
    cat << EOF > $TMP_ROOTFS_DIR/home/tvuser/my_autostart.sh
#!/bin/bash

# Remap Caps_Lock := Control_L
xmodmap -e 'remove Lock = Caps_Lock' \
        -e 'keysym Caps_Lock = Control_L' \
        -e 'add Control = Control_L'

sleep 2
teamviewer
EOF
    chmod +x $TMP_ROOTFS_DIR/home/tvuser/my_autostart.sh
    cat << EOF > $TMP_ROOTFS_DIR/home/tvuser/.config/autostart/my_autostart.desktop
[Desktop Entry]
Exec=/home/tvuser/my_autostart.sh
Name=my_autostart
Type=Application
Version=1.0
EOF
    mkdir -p $TMP_ROOTFS_DIR/home/tvuser/Desktop
    cp $TMP_ROOTFS_DIR/opt/teamviewer/tv_bin/desktop/com.teamviewer.TeamViewer.desktop \
        $TMP_ROOTFS_DIR/home/tvuser/Desktop/
    echo "+ sudo killall teamviewerd"
    if pgrep teamviewerd 1>/dev/null ; then
        sudo killall teamviewerd
        sleep 1
    fi
    echo "+ sudo umount $TMP_ROOTFS_DIR"
    sudo umount $TMP_ROOTFS_DIR
    mv $TMP_ROOTFS_FILE $ROOTFS_FILE
    rm -rf $TMP_DIR
fi

# Extract/update KERNEL and INITRD:
rm -f $KERNEL $INITRD
mkdir -p $FUSE_MOUNT
if mount | grep -q "^$ROOTFS_FILE on $FUSE_MOUNT type fuse " ; then
    fusermount -u $FUSE_MOUNT
fi
fuseext2 $ROOTFS_FILE $FUSE_MOUNT
cp $FUSE_MOUNT/vmlinuz $KERNEL
cp $FUSE_MOUNT/initrd.img $INITRD
fusermount -u $FUSE_MOUNT
rm -rf $FUSE_MOUNT

cleanup()
{
    set +e
    PID=$(cat $PID_FILE 2>/dev/null)
    if [ -n "$PID" ]; then
        kill $PID 2>/dev/null
    fi
}
trap cleanup INT EXIT

# See: https://www.linux-kvm.org/page/SPICE
GUI_ARGS=(
    -vga qxl -spice addr=127.0.0.1,port=5900,disable-ticketing
    -device virtio-serial-pci
    -device virtserialport,chardev=spicechannel0,name=com.redhat.spice.0
    -chardev spicevmc,id=spicechannel0,name=vdagent
    -daemonize -pidfile $PID_FILE
    -append 'root=/dev/sda rw'
)

CONSOLE_ARGS=(
    -nographic
    -append 'console=ttyS0 root=/dev/sda rw'
)

if [ -n "$NO_GUI" ]; then
    ARGS=("${CONSOLE_ARGS[@]}")
else
    ARGS=("${GUI_ARGS[@]}")
fi

echo "Starting qemu.."
qemu-system-x86_64 -m 4096 -smp 4 -enable-kvm \
    "${ARGS[@]}" \
    -kernel $KERNEL -initrd $INITRD -drive file=$ROOTFS_FILE,format=raw

    # See also: https://gist.github.com/shamil/62935d9b456a6f9877b5
    # -hda hdd.image.qcow2 # -cdrom $DISTRO_ISO -boot d

if [ -z "$NO_GUI" ]; then
    # spicy -h localhost -p 5900 # can't return keyboard focus to host OS

    # Use Ctrl_L+Alt_L to release keyboard (to return keyboard focus to host OS):
    echo "Starting remote-viewer.."
    remote-viewer -f spice://localhost:5900
    # --spice-debug --debug
fi
