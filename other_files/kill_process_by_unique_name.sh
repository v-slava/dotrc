#!/bin/bash

usage()
{
	echo "Usage: $(basename $0) PROCESS_NAME"
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi
PROCESS_NAME="$1"

PIDS=$(pidof "$PROCESS_NAME")

# No process found -> do nothing.
if [ -z "$PIDS" ]; then
	exit 0
fi

# Multiple processes found -> do nothing.
if echo $PIDS | grep -q ' ' ; then
	exit 0
fi

# $PIDS contains only one PID -> kill it.
kill $PIDS

