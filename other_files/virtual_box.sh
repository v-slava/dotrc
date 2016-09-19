#!/bin/bash

grep -q 'systemd\.unit=virtual\.target' /proc/cmdline

# xrandr | grep -q VGA-0
# systemctl status startx@virtual.service

