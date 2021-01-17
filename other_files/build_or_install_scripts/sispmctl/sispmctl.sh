#!/bin/bash

TMP="/media/files/workspace/tmp"

set -ex
sudo apt-get install libusb-dev git make gcc libtool m4 automake
mkdir -p "$TMP"
cd "$TMP"
rm -rf sispmctl
git clone https://git.code.sf.net/p/sispmctl/git sispmctl
cd sispmctl
git tag
git checkout release-4.8
./autogen.sh
./configure --enable-webless
make
sudo make install

cat << EOF | sudo tee /etc/udev/rules.d/60-sispmctl.rules
ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="04b4", \
ATTRS{idProduct}=="fd15", MODE="660", GROUP="plugdev"
EOF
sudo udevadm control --reload
# Now reconnect EG-PMS2 usb device.

# On raspberry pi use (bug?):
# LD_LIBRARY_PATH=/usr/local/lib sispmctl

# scan for supported devices:
# sispmctl -s

# Turn on outlet 1:
# sispmctl -q -o 1

# Turn off outlet 4:
# sispmctl -q -f 4

# Turn on outlets 1, 2 and 4:
# sispmctl -q -o 1 -o 2 -o 4

# Turn off all outlets:
# sispmctl -q -f all

# Print status of outlet 2 (on|off):
# sispmctl -q [-g|-m] 2
