#!/bin/bash

CURRENT_LAYOUT=$(setxkbmap -query | grep 'layout:' | cut -d' ' -f6)

set -e

if [ "$CURRENT_LAYOUT" = "us,ru" ]; then
	setxkbmap -layout de,ru
	zenity --info --title "us <--> de keyboard layout switcher" \
		--text "Successfully switched to \"de\" keyboard layout."
else
	if [ "$CURRENT_LAYOUT" = "de,ru" ]; then
		setxkbmap -layout us,ru
		xmodmap ~/.Xmodmap
		zenity --info --title "us <--> de keyboard layout switcher" \
			--text "Successfully switched to \"us\" keyboard layout."
	else
		zenity --error --title "us <--> de keyboard layout switcher" \
			--text "$0:\nFailed to determine current keyboard layout."
	fi
fi

