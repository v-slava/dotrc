#!/bin/bash

# Use "git remote add" to add $REPOSITORY

USAGE="$(basename $0) REPOSITORY BRANCH"

usage()
{
    echo $USAGE 1>&2
    exit 1
}

if [ $# -ne 2 ]; then
    usage
fi

REPOSITORY=$1
BRANCH=$2

set -e
