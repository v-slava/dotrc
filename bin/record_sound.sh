#!/bin/bash

set -e

USAGE="Usage: $(basename $0) {-m|-s} {--wav|--mp3|--ogg} FILE

-m
    Record from microphone.
-s
    Record from speakers.

Note: FILE should be without extension.
"

usage()
{
    echo -e "$USAGE"
    exit 1
}

if [ $# -ne 3 ] ; then
    usage
fi

MODE="$1"
shift
FORMAT="$1"
shift
FILE="$1"
shift

case "$MODE" in
    ("-m")
        DEVICE="$($DOTRC/other_files/get_source_name.sh)"
        # Note: in this case we could also omit specifying device for parec.
        ;;
    ("-s") DEVICE="$($DOTRC/other_files/get_sink_name.sh).monitor" ;;
    (*) usage ;;
esac

# To record output sound only (either speaker or bluetooth headset):
case "$FORMAT" in
    ("--wav") parec -d "$DEVICE" --file-format=wav "${FILE}.wav" ;;
    ("--mp3") parec -d "$DEVICE" | lame -r - "${FILE}.mp3" ;; # --cbr -b 320
    ("--ogg") parec -d "$DEVICE" | oggenc -b 192 -o "${FILE}.ogg" --raw - ;;
    (*) usage ;;
esac

# To record both input and output sound:
# http://www.maartenbaert.be/simplescreenrecorder/recording-game-audio/

# $ pactl load-module module-null-sink sink_name=duplex_out
# $ pactl load-module module-null-sink sink_name=game_out
# $ pactl load-module module-loopback source=game_out.monitor
# $ pactl load-module module-loopback source=game_out.monitor sink=duplex_out
# $ pactl load-module module-loopback sink=duplex_out
# $ pavucontrol
# Setup for "Playback" second null output, for "Recording" - first null output.
# Now record sound:
# $ parec

