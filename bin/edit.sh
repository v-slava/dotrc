#!/bin/bash

if ! pgrep emacs ; then
	emacs --daemon
fi
emacsclient -c -n "$@"

