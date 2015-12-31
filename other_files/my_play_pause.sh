#!/bin/bash

# Due to some bug in alsaplayer, there is a playback slowdown after
# pause when playing 48 kHz mp3.
# To avoid this issue force speed to 100% on play.

alsaplayer --pause
if ! alsaplayer --status | grep -q 'speed: 0%' ; then
	alsaplayer --speed 1
fi

