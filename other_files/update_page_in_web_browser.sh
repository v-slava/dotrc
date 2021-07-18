#!/bin/bash

PREV_MARK=PREVIOUS_WINDOW
BROWSER_MARK=browser

set -e

sleep 0.01 # sometimes browser doesn't detect that html has been updated..

# The following doesn't work in sway:
# swaymsg "mark --add $PREV_MARK, [con_mark=\"$BROWSER_MARK\"] focus"
swaymsg "mark --add $PREV_MARK"
swaymsg "[con_mark=\"$BROWSER_MARK\"] focus"

# xdotool key F5

# The following doesn't work for me:
# wtype -k f5
# wtype -M ctrl
# wtype l
# wtype -m ctrl

sudo ydotool key f5

swaymsg "[con_mark=\"$PREV_MARK\"] focus"
