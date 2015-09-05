#!/bin/bash

set -ex

# parec -d alsa_output.pci-0000_00_1b.0.analog-stereo.monitor | oggenc -b 192 -o ~/record.ogg --raw -
parec -d alsa_output.pci-0000_00_1b.0.analog-stereo.monitor | lame -r - ~/record.mp3

