#!/bin/bash

IN_FILE=~/os_settings/home_settings/.config_xdg/i3/config
OUT_FILE=~/.config_xdg/i3/config

~/os_settings/other_files/virtual_box.sh
VIRTUAL=$?

if [ $VIRTUAL -eq 1 ] ; then
	# Native [begin ; '# SED workspace virtual begin'], ['# SED workspace virtual end' ; end]
	VIRTUAL_BEGIN=$(grep -n '# SED workspace virtual begin' $IN_FILE | cut -d':' -f1)
	VIRTUAL_END=$(grep -n '# SED workspace virtual end' $IN_FILE | cut -d':' -f1)

	head "$IN_FILE" -n $VIRTUAL_BEGIN > "$OUT_FILE"
	tail "$IN_FILE" -n +$VIRTUAL_END >> "$OUT_FILE"
else
	# Virtual Box [begin ; '# SED workspace native begin'], ['# SED workspace native end' ; end]
	NATIVE_BEGIN=$(grep -n '# SED workspace native begin' $IN_FILE | cut -d':' -f1)
	NATIVE_END=$(grep -n '# SED workspace native end' $IN_FILE | cut -d':' -f1)

	head "$IN_FILE" -n $NATIVE_BEGIN > "$OUT_FILE"
	tail "$IN_FILE" -n +$NATIVE_END >> "$OUT_FILE"
fi
i3-msg reload

