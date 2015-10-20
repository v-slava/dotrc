#!/bin/bash

set -e

SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"

echo "Set volume to 100% when recording: otherwise output volume will be low"

# parec -d "$SINK_NAME.monitor" | oggenc -b 192 -o ~/record.ogg --raw -
parec -d "$SINK_NAME.monitor" | lame -r - ~/record.mp3

