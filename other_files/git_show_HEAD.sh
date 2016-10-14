#!/bin/bash

CMD="git show"
if [ "$1" = "-n" ]; then
	CMD="$CMD --name-only"
	shift
fi

if [ -z "$1" ]; then
	$CMD HEAD
else
	$CMD HEAD~$1
fi

