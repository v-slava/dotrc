#!/usr/bin/env bash

ls --quoting-style=shell *.mp3 | sort -R | xargs $DOTRC/other_files/audio_player_play_files.sh
