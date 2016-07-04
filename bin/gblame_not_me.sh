#!/bin/bash

set -e

MY_NAME="$(git config user.name)"
REGEX="^[^ ]\+ (${MY_NAME}"

find -type f | while read file ; do
	if git blame "$file" | grep -v -q "$REGEX" ; then
		echo "File: \"$file\""
		git blame "$file" | grep -v "$REGEX"
	fi
done

