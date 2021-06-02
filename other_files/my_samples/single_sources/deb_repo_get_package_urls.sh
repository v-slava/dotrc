#!/bin/bash

# From /etc/apt/sources.list:
# # base repository:
# deb http://deb.debian.org/debian bullseye main contrib non-free
# deb-src http://deb.debian.org/debian bullseye main contrib non-free
BASE_URL=http://deb.debian.org/debian
DISTRO=bullseye
COMPONENT=main

# Add the following line in /etc/apt/sources.list
# deb [arch=amd64] https://repo.fortinet.com/repo/6.4/ubuntu/ /bionic multiverse
BASE_URL=https://repo.fortinet.com/repo/6.4/ubuntu
DISTRO=bionic
COMPONENT=multiverse

PACKAGES_GZ=$BASE_URL/dists/$DISTRO/$COMPONENT/binary-amd64/Packages.gz
# FILE_NAME=$(curl -s $PACKAGES_GZ | gunzip | grep '^Filename: ' | grep '/vim_' \
#     | cut -d' ' -f2)
# echo $FILE_NAME
# # pool/main/v/vim/vim_8.2.2434-3_amd64.deb
# # pool/multiverse/forticlient/forticlient_6.4.4.0984_amd64.deb
# wget $BASE_URL/$FILE_NAME
curl -s $PACKAGES_GZ | gunzip | grep '^Filename: ' | cut -d' ' -f2 \
    | sed -e "s|^|$BASE_URL/|"
