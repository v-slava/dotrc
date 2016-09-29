#!/bin/bash

LOCK_DIR=/tmp/system_update_lock_dir
if ! mkdir "$LOCK_DIR" ; then
	echo -e "Error: lock directory exists
(system update has already been started in parallel process)."
	exit 1
fi
set -x
sudo update-flashplugin-nonfree --install
sudo update-pepperflashplugin-nonfree --install
sudo apt-get update
sudo apt-get upgrade --yes
sync
rm -rf "$LOCK_DIR"
