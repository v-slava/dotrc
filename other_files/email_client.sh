#!/bin/bash

set -e

sync_mail()
(
    set -e
    if ! pidof mbsync ; then
        mbsync -a
    fi
)

if [ "$1" = "--startup" ]; then
    while ! ping -c 1 -W 1 8.8.8.8 ; do
        echo "Waiting for 8.8.8.8 (network interface might be down)..."
        sleep 1
    done
    sync_mail
fi

# In this case "push <first-entry>" doesn't work:
# $DOTRC/other_files/open_terminal.sh neomutt

$DOTRC/other_files/open_terminal.sh bash -i -c \
    'echo -ne "\033]0;neomutt\007" && cd ~/downloads && neomutt && true'

sync_mail
exit 0

while [ true ]; do
    thunderbird
    if pidof thunderbird 1>/dev/null ; then
        break
    fi
    zenity --error --text '<span foreground="red" font="32">Do not close thunderbird window!</span>\n\n<i>It will be restarted automatically for now</i>'
done
