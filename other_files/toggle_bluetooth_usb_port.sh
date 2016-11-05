#!/bin/bash

USB_PORT=$(udevadm trigger -v -s bluetooth | head -n1 | grep -o 'usb.*$' | cut -d'/' -f3)

echo "$USB_PORT" > /sys/bus/usb/drivers/usb/unbind
# sleep 1
echo "$USB_PORT" > /sys/bus/usb/drivers/usb/bind

