#!/bin/bash

set -e
if [ "$1" = "-r" ]; then
    echo "+ svn revert -R -- ."
    svn revert -R -- .
fi
svn status --no-ignore | grep '^[?I!A]      ' | grep -v '^?       \.git$' \
    | grep -v '^?       \.gitignore$' | cut -d' ' -f8- | xargs rm -rf
echo "+ svn status --no-ignore"
svn status --no-ignore
echo "+ svn status --no-ignore: exit code=$?"
