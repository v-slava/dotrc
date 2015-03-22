#!/usr/bin/env bash

set -ex

# Signal bash to include filenames beginning with a `.'
# in the results of pathname expansion:
shopt -s dotglob

cp -rafv $PWD/home_settings/* $HOME/

if [ -n "$DISPLAY" ]; then
	xrdb ~/.Xresources
fi

if [ ! -L ~/bin ]; then
	ln -s $PWD/bin ~/bin
fi
if [ ! -L ~/os_settings ]; then
	ln -s $PWD ~/os_settings
fi
if [ ! -d ~/terminal ]; then
	mkdir ~/terminal
fi

MEDIA_FILES=/media/files
DIRS="downloads other temporary workspace"
for dir in $DIRS ; do
	if [ ! -L ~/$dir ]; then
		ln -s $MEDIA_FILES/$dir ~/$dir
	fi
done

