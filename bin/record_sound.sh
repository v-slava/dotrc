#!/bin/bash

set -ex

SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"

# parec -d "$SINK_NAME" | oggenc -b 192 -o ~/record.ogg --raw -
parec -d "$SINK_NAME" | lame -r - ~/record.mp3

