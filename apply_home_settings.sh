#!/usr/bin/env bash

if [ -n "$WINDIR" ]; then
    echo -e "This script is not applicable for windows." 1>&2
    read -p "Press enter to continue..."
    exit 1
fi

set -ex

# Signal bash to include filenames beginning with a `.'
# in the results of pathname expansion:
shopt -s dotglob

cp -rafv $PWD/home_settings/* $HOME/

if [ -n "$DISPLAY" ]; then
    xrdb ~/.Xresources
fi

if [ ! -e ~/my ]; then
    ln -s /media/files/temporary/my ~/my
fi
if [ ! -e ~/bin ]; then
    ln -s $PWD/bin ~/bin
fi
if [ ! -e ~/downloads ]; then
    ln -s /media/files/downloads ~/downloads
fi
if [ ! -e ~/Downloads ]; then
    ln -s /media/files/downloads ~/Downloads
fi
if [ ! -e ~/Desktop ]; then
    ln -s /media/files/downloads ~/Desktop
fi
if [ ! -e ~/os_settings ]; then
    ln -s $PWD ~/os_settings
fi
if [ ! -e ~/.spacemacs ]; then
    ln -s $PWD/other_files/dot_spacemacs.el ~/.spacemacs
fi
# if [ ! -d ~/terminal ]; then
#     mkdir ~/terminal
# fi

MEDIA_FILES=/media/files
DIRS="downloads temporary workspace"
for dir in $DIRS ; do
    if [ ! -L ~/$dir ]; then
        ln -s $MEDIA_FILES/$dir ~/$dir
    fi
done

MY_DIRS="other"
for dir in $MY_DIRS ; do
    if [ ! -L ~/my/$dir ]; then
        ln -s $MEDIA_FILES/$dir ~/my/$dir
    fi
done

set +e
if [ -d /media/files/workspace/dotrc_s/home_settings ]; then
    set -e
    cd /media/files/workspace/dotrc_s/home_settings
    FILES_LIST=$(find -type f)
    for FILE in $FILES_LIST ; do
        cat "$FILE" >> ~/$FILE
    done
fi
set -e

~/os_settings/other_files/generate_configs.sh
i3-msg reload
if [ "$(id -u)" != "0" ]; then
    systemctl --user enable rdm.socket
fi

echo -e "\nDone!"
