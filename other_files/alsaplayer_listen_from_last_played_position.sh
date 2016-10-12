#!/bin/bash

# Create a symbolic link to this file in folder you are playing and run it
# from this folder.

# alsaplayer --status | grep 'path: ' | cut -d' ' -f2 > last_played_position.txt
# alsaplayer --status | grep 'position: ' | cut -d' ' -f2 >> last_played_position.txt

set -e

# Play again what we've already heard:
REPLAY_LAST_SECONDS=10

FILE=$(head -n 1 last_played_position.txt)
POSITION=$(tail -n 1 last_played_position.txt)
POSITION=$((POSITION - REPLAY_LAST_SECONDS))

~/os_settings/other_files/alsaplayer_play_files.sh "$FILE"
sleep 1
alsaplayer --seek $POSITION

