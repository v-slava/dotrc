#!/bin/bash

PIPEWIRE_DEBUG=3 pipewire &
PIPEWIRE_PID=$!
sleep 1
/usr/libexec/xdg-desktop-portal-wlr -l DEBUG
