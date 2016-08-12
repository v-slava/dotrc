#!/bin/bash

set -e

USAGE="Usage: $(basename $0) {--wav|--mp3|--ogg} FILE

Note: FILE should be without extension.

Set volume to 100% when recording mp3.
Otherwise output volume will be low.
"

usage()
{
	echo -e "$USAGE"
	exit 1
}

if [ $# -ne 2 ] ; then
	usage
fi

SINK_NAME="$(~/os_settings/other_files/get_sink_name.sh)"
FILE="$2"

# To record input sound (from microphone) only:
# $ parec -d alsa_input.pci-0000_00_1b.0.analog-stereo

# To record output sound only (either speaker or bluetooth headset):
case "$1" in
	("--wav") parec -d "$SINK_NAME.monitor" --file-format=wav "${FILE}.wav" ;;
	("--mp3") parec -d "$SINK_NAME.monitor" | lame -r - "${FILE}.mp3" ;; # --cbr -b 320
	("--ogg") parec -d "$SINK_NAME.monitor" | oggenc -b 192 -o "${FILE}.ogg" --raw - ;;
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

