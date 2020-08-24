#!/bin/bash

set -e

# gtf 1366 768 60
xrandr --newmode 1366x768 85.86 1368 1440 1584 1800 768 769 772 795 -HSync +Vsync
xrandr --addmode DVI-0 1366x768
# xrandr --output DVI-0 --mode 1366x768
/home/elena/resolution.sh &
