#!/bin/bash

# LOCK_DIR=/tmp/system_update_lock_dir
# if ! mkdir "$LOCK_DIR" ; then
# 	echo -e "Error: lock directory exists
# (system update has already been started in parallel process)."
# 	exit 1
# fi
set -x
# sudo update-flashplugin-nonfree --install
# sudo update-pepperflashplugin-nonfree --install
sudo apt-get clean
sudo apt-get update
sudo apt-get upgrade --yes
if which youtube-dl 1>/dev/null ; then
    pip3 install --user --upgrade youtube_dl
fi
sync
# rm -rf "$LOCK_DIR"
