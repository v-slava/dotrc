#!/bin/bash

# Get svn branch by SVN revision (commit)

usage()
{
    echo "Usage: $(basename $0) SVN_REVISION" 1>&2
    exit 1
}

REVISION="$1"
if [ -z "$REVISION" ]; then
    usage
fi

REPO_ROOT="$(svn info . | grep '^Repository Root: ' | cut -d' ' -f 3-)"
svn log -v -q "$REPO_ROOT" -r "$REVISION" -l 1
