#!/bin/bash

# apt-get install dnsmasq
# systemctl disable dnsmasq.service
# systemctl stop dnsmasq.service

# slava laptop= NOPASSWD: /media/files/workspace/dotrc/other_files/start_router.sh
# exec --no-startup-id exec $START sudo $DOTRC/other_files/start_router.sh

set -ex

INET_IF=wlp3s0
SRV_IF=enp2s0f2

killall -q dnsmasq || true

ifconfig $SRV_IF 172.30.1.1

dnsmasq -i $SRV_IF --dhcp-range=172.30.0.2,172.30.0.254,12h

sysctl net.ipv4.ip_forward=1

iptables -F
iptables -X

iptables -t nat -A POSTROUTING -o $SRV_IF -j MASQUERADE
iptables -A FORWARD -i $SRV_IF -o $INET_IF -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i $INET_IF -o $SRV_IF -j ACCEPT

iptables -t nat -A POSTROUTING -o $INET_IF -j MASQUERADE
iptables -A FORWARD -i $INET_IF -o $SRV_IF -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i $SRV_IF -o $INET_IF -j ACCEPT

echo "Success!"
