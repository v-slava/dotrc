#!/bin/bash

alsaplayer -E "$@"
if alsaplayer --status | grep -q 'speed: 0%' ; then
	$DOTRC/other_files/my_play_pause.sh
fi

