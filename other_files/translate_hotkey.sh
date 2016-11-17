#!/bin/bash

# echo 'er: english -> russian
# re: russian -> english
# dr: deutsch -> russian
# rd: russian -> deutsch' | dmenu -fn 'Inconsolata LGC-16:monospace' -p 'translate from:'

# Note: i3-wm version 4.10+ required for wmctrl to work here.

goldendict &

set -e

REQUEST=$(dmenu -fn 'Inconsolata LGC-16:monospace' -p 'word to translate:' < /dev/null)

WIN_IDs=$(wmctrl -l | grep GoldenDict | cut -d' ' -f1)
for win_id in $WIN_IDs ; do
	wmctrl -ic $win_id
done

LANG_PAIR=$(echo $REQUEST | cut -c -2)
WORD="$(echo $REQUEST | cut -c 4-)"

# List of language pairs and corresponding lang_num:
# All       (LANG_NUM = 0)
# En - En   (LANG_NUM = 1)
# En - Ru   (LANG_NUM = 2)
# Ru - En   (LANG_NUM = 3)
# De - Ru   (LANG_NUM = 4)
# Ru - De   (LANG_NUM = 5)

case "$LANG_PAIR" in
	"er" ) LANG_NUM=2 ;;
	"re" ) LANG_NUM=3 ;;
	"dr" ) LANG_NUM=4 ;;
	"rd" ) LANG_NUM=5 ;;
	* ) zenity --error --title translate --text "Wrong language pair has been given.\n\
Accepted values are: er, re, dr, rd." ; exit 1 ;;
esac

echo "$WORD" | clipboard.sh
goldendict "$WORD"
# select first language pair ("All", LANG_NUM = 0)
xdotool keyup Alt
xdotool key Alt+g
xdotool search goldendict key Up Up Up Up Up

i=1
while [ $i -le $LANG_NUM ]; do
	((i++))
	xdotool search goldendict key Down
done
xdotool search goldendict key Return

# Focus goldendict and switch to floating layout
wmctrl -a goldendict
xdotool key Alt_L+shift+space

