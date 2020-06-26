#!/bin/bash

if pgrep skypeforlinux 1>/dev/null ; then
    killall skypeforlinux
fi

if pgrep viber 1>/dev/null ; then
    killall -9 viber
fi

if pgrep telegram 1>/dev/null ; then
    killall telegram
fi

$DOTRC/other_files/stop_email_notifications.sh
