#!/bin/bash

alsaplayer -E "$@"
if alsaplayer --status | grep -q 'speed: 0%' ; then
	~/os_settings/other_files/my_play_pause.sh
fi

