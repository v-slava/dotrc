#!/bin/bash

# Create a symbolic link to this file in folder you are playing and run it
# from this folder.

set -e

alsaplayer --status | grep 'path: ' | cut -d' ' -f2 > last_played_position.txt
alsaplayer --status | grep 'position: ' | cut -d' ' -f2 >> last_played_position.txt

