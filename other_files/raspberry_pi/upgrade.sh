#!/bin/bash

set -e

sudo apt-get clean
sudo apt-get update
sudo apt-get upgrade --yes
sync
sudo reboot
