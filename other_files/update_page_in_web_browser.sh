#!/bin/bash

PREV_MARK=PREVIOUS_WINDOW
BROWSER_MARK=browser

set -e

# xdotool search {--class|--classname|--name} google-chrome key F5

source $DOTRC/other_files/i3_msg.sh
i3_msg "mark --add $PREV_MARK, [con_mark=\"$BROWSER_MARK\"] focus"
xdotool key F5
i3_msg "[con_mark=\"$PREV_MARK\"] focus"
