#!/bin/bash

# exec vim "$@"

if ! pgrep emacs 1>/dev/null ; then
	emacs --daemon
fi

CMD="emacsclient -c"
if [ "$1" = "--wait" ] || [ "$1" = "--nofork" ]; then
	shift
else
	CMD="$CMD -n"
fi

$CMD "$@"

