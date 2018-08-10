#!/bin/bash

# set -e

# Move application producing sound to default sink:
SINK_INPUT_INDICES=$(LANG=C pacmd list-sink-inputs | grep 'index: ' | cut -d':' -f2 | cut -d' ' -f2)
for sink_input_index in $SINK_INPUT_INDICES ; do
    pactl move-sink-input $sink_input_index @DEFAULT_SINK@
done

SOURCE_OUTPUT_INDICES=$(LANG=C pacmd list-source-outputs | grep 'index: ' | cut -d':' -f2 | cut -d' ' -f2)
for source_output_index in $SOURCE_OUTPUT_INDICES ; do
    pactl move-source-output $source_output_index @DEFAULT_SOURCE@
done
