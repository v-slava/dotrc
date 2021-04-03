#!/bin/bash

# LOCK_DIR=/tmp/system_update_lock_dir
# if ! mkdir "$LOCK_DIR" ; then
# 	echo -e "Error: lock directory exists
# (system update has already been started in parallel process)."
# 	exit 1
# fi
# set -x
# sudo update-flashplugin-nonfree --install
# sudo update-pepperflashplugin-nonfree --install
set -e
echo "+ sudo apt-get clean"
sudo apt-get clean
echo "+ sudo apt-get update"
set +e
sudo apt-get update
set -e
echo "+ sudo apt-get upgrade --with-new-pkgs --yes"
sudo apt-get upgrade --with-new-pkgs --yes
echo "+ sudo apt-get autoremove --purge --yes"
sudo apt-get autoremove --purge --yes
if which pip3 1>/dev/null ; then
    # pip3 install --user --upgrade
    PACKAGES="$(pip3 list | tail -n +3 | cut -d' ' -f1 | tr '\r\n' ' ')"
    echo "+ pip3 install --upgrade $PACKAGES"
    pip3 install --upgrade $PACKAGES
fi
sync
# rm -rf "$LOCK_DIR"
