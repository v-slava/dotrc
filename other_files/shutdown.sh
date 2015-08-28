#!/bin/bash

set -x

sudo update-flashplugin-nonfree --install
sudo update-pepperflashplugin-nonfree --install
sync
sudo poweroff

