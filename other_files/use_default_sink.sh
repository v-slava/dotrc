#!/bin/bash

# Move application producing sound to default sink:
SINK_INPUT_INDICES=$(pacmd list-sink-inputs | grep 'index: ' | cut -d':' -f2 | cut -d' ' -f2)
for sink_input_index in $SINK_INPUT_INDICES ; do
    pactl move-sink-input $sink_input_index @DEFAULT_SINK@
done
