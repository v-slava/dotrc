#!/bin/bash

# This script returns 0 if we are running under VirtualBox and non-zero otherwise.

# grep -q 'systemd\.unit=virtual\.target' /proc/cmdline
grep -q 'my_virtualbox' /proc/cmdline

# xrandr | grep -q VGA0
# systemctl status startx@virtual.service

