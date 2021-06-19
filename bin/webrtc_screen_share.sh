#!/bin/bash

# https://www.screenleap.com
# Enable: chrome://flags/#enable-webrtc-pipewire-capturer

PIPEWIRE_DEBUG=3 pipewire &
PIPEWIRE_PID=$!
sleep 1

cleanup()
{
    kill $PIPEWIRE_PID
}
trap cleanup INT EXIT

/usr/libexec/xdg-desktop-portal-wlr -l DEBUG
