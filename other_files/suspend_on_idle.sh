#!/bin/bash

# Execute checks every LOOP_TIME (in seconds) and go to sleep mode if all of the
# following conditions are met:
# - there were no user input for NO_USER_INPUT_TIME
# - the system was silent (no output sound) for SILENT_TIME

set -e

let LOOP_TIME=12*60
let NO_USER_INPUT_TIME=10*60
SILENT_TIME=20

SOUND=/tmp/record_idle
SOUND_FILE="${SOUND}.wav"
let NO_USER_INPUT_TIME=$NO_USER_INPUT_TIME*1000

while [ true ] ; do
	sleep $LOOP_TIME

	# Do not go to sleep mode if skype is running:
	if pgrep skype 1>/dev/null ; then
		continue
	fi

	# Preliminary check: do not do anything if there was user input recently
	if [ $(xprintidle) -le $NO_USER_INPUT_TIME ]; then
		continue
	fi

	# Determine whether we will be silent for next SILENT_TIME:
	record_sound.sh --wav "$SOUND" &
	sleep "$SILENT_TIME"
	killall parec
	if ! sox "$SOUND_FILE" -n stats 2>&1 | grep 'Pk lev dB' | grep -q '\-inf' ; then
		continue
	fi

	# Final check: do not do anything if there was user input recently
	if [ $(xprintidle) -le $NO_USER_INPUT_TIME ]; then
		continue
	fi

	x-terminal-emulator -title "suspend script" -e bash -c \
'source ~/.bashrc && $DOTRC/other_files/update_system.sh && exec sudo systemctl suspend'
done

