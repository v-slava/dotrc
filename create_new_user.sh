#!/usr/bin/env bash

# To delete existed user use the following command:
# deluser --remove-home USER_NAME

set -e

if [ $(whoami) != root ]; then
	echo "Error: this script must be launched as root" 1>&2
	exit 1
fi

if [ $# -ne 1 ]; then
	echo "Usage: $0 USER_NAME" 1>&2
	exit 1
fi

USER_NAME=$1
USER_PASSWD=$USER_NAME

# echo -e "$USER_PASSWD\n$USER_PASSWD\n\n\n\n\n\ny" | adduser $USER_NAME
useradd -m -s /bin/bash $USER_NAME
echo "${USER_NAME}:${USER_PASSWD}" | chpasswd

USER_GROUPS=sudo,audio,video,systemd-journal,netdev,plugdev,bluetooth,docker
if ! usermod -a -G $USER_GROUPS $USER_NAME ; then
	echo "Failed to add user to groups!" 1>&2
	exit 1
fi

# usermod -a -G vboxsf $USER_NAME
