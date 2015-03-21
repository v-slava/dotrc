#!/usr/bin/env bash

set -e

ORIGINAL_FILE=$(ls ~/downloads/*.torrent)
NEW_FILE=/tmp/$(basename $ORIGINAL_FILE)
mv $ORIGINAL_FILE $NEW_FILE
exec transmission-gtk $NEW_FILE

