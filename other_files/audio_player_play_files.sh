#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $(basename $0) FILE ..." 1>&2
    exit 1
fi

# Check if previous instance of "xmms2 add -f ..." runs in background.
# If so - kill it before we start here again.
PID=$(pgrep '^xmms2$')
if [ -n "$PID" ]; then
    kill $PID
fi

if [[ $PWD == /media/files/music/new* ]]; then
    for file in "$@" ; do
        if ! id3v2 -l "$file" | grep -q ': No ID3 tag$' ; then
#             cat << EOF 1>&2
# Error: ID3 tags found for $file. Delete tags by running:
#
# id3v2 -D "$(realpath "$file")"
#
# and try playing it again
# EOF
#             exit 1
            # echo "Removing ID3 tags for $file ..."
            id3v2 -D "$file"
        fi
    done
fi

cat << EOF | xmms2 > /dev/null
stop
remove *
add -f "$1"
play
EOF
shift
if [ $# -ne 0 ]; then
    xmms2 add -f "$@" &
fi
# playlist clear
exit

alsaplayer -E "$@"
if alsaplayer --status | grep -q 'speed: 0%' ; then
	$DOTRC/other_files/my_play_pause.sh
fi

