#!/bin/bash

if [ -z "$1" ]; then
	git show HEAD
else
	git show HEAD~$1
fi

