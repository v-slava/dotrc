#!/bin/bash

set -e

MY_NAME="$(git config user.name)"
find -type f | while read file ; do
	git blame "$file" | grep -v "^[^ ]\+ (${MY_NAME}"
done

