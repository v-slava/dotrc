#!/usr/bin/env bash

ls --quoting-style=shell *.mp3 | sort -R | xargs $DOTRC/other_files/alsaplayer_play_files.sh
