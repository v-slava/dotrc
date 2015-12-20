#!/bin/bash

set -x

sudo update-flashplugin-nonfree --install
sudo update-pepperflashplugin-nonfree --install
sudo apt-get update
sudo apt-get upgrade --yes
sync
