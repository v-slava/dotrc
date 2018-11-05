#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $(basename $0) FILE ..." 1>&2
    exit 1
fi

cat << EOF | xmms2 > /dev/null
stop
remove *
add -f $1
play
EOF
shift
xmms2 add -f "$@"
# playlist clear
exit

alsaplayer -E "$@"
if alsaplayer --status | grep -q 'speed: 0%' ; then
	$DOTRC/other_files/my_play_pause.sh
fi

