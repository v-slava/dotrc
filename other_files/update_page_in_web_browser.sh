#!/bin/bash

PREV_MARK=PREVIOUS_WINDOW
BROWSER_MARK=browser

set -e

swaymsg "mark --add $PREV_MARK, [con_mark=\"$BROWSER_MARK\"] focus"
# xdotool key F5
wtype -k F5
swaymsg "[con_mark=\"$PREV_MARK\"] focus"
