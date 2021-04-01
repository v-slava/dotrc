#!/bin/bash

set -e

SINK=$(LANG=C pactl list sinks | grep '^.Name: ' | grep 'bluez_sink' \
    | cut -d' ' -f2)
LANG=C pactl list sink-inputs | grep '^Sink Input #' | cut -d'#' -f2 | \
while read SINK_INPUT ; do
    pactl move-sink-input $SINK_INPUT $SINK
done

SOURCE=$(LANG=C pactl list sources | grep '^.Name: ' | grep 'bluez_source' \
    | cut -d' ' -f2)
set +e
LANG=C pactl list source-outputs | grep '^Source Output #' | cut -d'#' -f2 | \
while read SOURCE_OUTPUT ; do
    # The following command may fail for some sources, we ignore these failures.
    pactl move-source-output $SOURCE_OUTPUT $SOURCE
done
true
