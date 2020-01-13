#!/bin/bash

NUM_SPACES_FOR_TAB=8
if [ -n "$1" ]; then
    NUM_SPACES_FOR_TAB="$1"
fi
TAB="$(printf %${NUM_SPACES_FOR_TAB}s)"
sed -e "s/\t/${TAB}/g" | grep -n '.\{81\}'
