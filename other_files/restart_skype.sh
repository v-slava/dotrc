#!/bin/bash

set -e

if pidof skypeforlinux 1>/dev/null ; then
    killall skypeforlinux
    while pidof skypeforlinux 1>/dev/null ; do
        sleep 0.2
    done
fi

$DOTRC/other_files/start_singleton_gui_program.py skype
