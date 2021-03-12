#!/bin/bash

# sudo apt-get install dnsmasq ifmetric
# sudo systemctl disable dnsmasq.service
# sudo systemctl stop dnsmasq.service
#
# slava laptop= NOPASSWD: /media/files/workspace/dotrc/other_files/start_router.sh
# exec --no-startup-id exec $START sudo $DOTRC/other_files/start_router.sh

# sudo nvim /etc/dnsmasq.conf # dhcp-range, interface
# sudo systemctl enable dnsmasq.service
# sudo cp router.service /etc/systemd/system/
# systemctl enable router.service
# systemctl start router.service
#
# [Unit]
# Description=routing packets from one interface to another
# After=network.target
#
# [Service]
# Type=oneshot
# ExecStart=/path/to/start_router.sh wlp3s0 enp2s0f2 -i 10.70.126.162 --systemd
#
# [Install]
# WantedBy=default.target

error()
{
    echo -e "$1" 1>&2
    exit 1
}

usage()
{
    error "Usage: $(basename $0) INET_IF SRV_IF [-i MY_IP] \
{-r DHCP_RANGE|--systemd}\n\nFor example:\n\
$(basename $0) wlan0 eth0 -i 10.70.126.162 -r 10.70.126.1,10.70.126.159\n\
$(basename $0) wlp3s0 enp2s0f2 -i 10.70.126.162 --systemd\n\
"
}

INET_IF="$1"
shift
SRV_IF="$1"
shift

if [ -z "$INET_IF" ] || [ -z "$SRV_IF" ]; then
    usage
fi

if ! ifconfig -a | grep -q "$INET_IF" ; then
    usage
fi

if ! ifconfig -a | grep -q "$SRV_IF" ; then
    usage
fi

while true; do
    if [ $# -eq 0 ]; then
        break
    fi
    case "$1" in
        "-i")
            shift
            MY_IP="$1"
            if [ -z "$MY_IP" ]; then
                usage
            fi
            ;;
        "-r")
            shift
            DHCP_RANGE="$1"
            if [ -z "$DHCP_RANGE" ]; then
                usage
            fi
            ;;
        "--systemd") SYSTEMD=true ;;
        *) usage ;;
    esac
    shift
done

if [ -z "$SYSTEMD" ] && [ -z "$DHCP_RANGE" ]; then
    usage
fi
if [ -n "$SYSTEMD" ] && [ -n "$DHCP_RANGE" ]; then
    usage
fi

set -e
# set -x

if [ -n "$MY_IP" ]; then
    ifconfig $SRV_IF $MY_IP
fi

if [ -z "$SYSTEMD" ]; then
    # Set metric to 0 = highest priority
    ifmetric $INET_IF
    # TODO how to set it in --systemd mode?

    killall -q dnsmasq || true
    # 12h is "lease time"
    dnsmasq -q -i $SRV_IF --dhcp-range=$DHCP_RANGE,12h
    # See: cat /var/lib/misc/dnsmasq.leases
fi

sysctl net.ipv4.ip_forward=1

iptables -F
iptables -X

iptables -t nat -A POSTROUTING -o $SRV_IF -j MASQUERADE
iptables -A FORWARD -i $SRV_IF -o $INET_IF -m state \
    --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i $INET_IF -o $SRV_IF -j ACCEPT

iptables -t nat -A POSTROUTING -o $INET_IF -j MASQUERADE
iptables -A FORWARD -i $INET_IF -o $SRV_IF -m state \
    --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i $SRV_IF -o $INET_IF -j ACCEPT

echo "Success!"
