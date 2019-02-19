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

MEDIA_FILES=/media/files

cp -rafv $PWD/home_settings/* $HOME/

if [ -n "$DISPLAY" ]; then
    xrdb ~/.Xresources
fi

if [ ! -e ~/my ]; then
    ln -s $MEDIA_FILES/temporary/my ~/my
fi
if [ ! -e ~/bin ]; then
    ln -s $PWD/bin ~/bin
fi
if [ ! -e ~/downloads ]; then
    ln -s $MEDIA_FILES/downloads ~/downloads
fi
if [ ! -e ~/Downloads ]; then
    ln -s $MEDIA_FILES/downloads ~/Downloads
fi
if [ ! -e ~/Desktop ]; then
    ln -s $MEDIA_FILES/downloads ~/Desktop
fi
if [ ! -e ~/.spacemacs ]; then
    ln -s $PWD/other_files/dot_spacemacs.el ~/.spacemacs
fi
# if [ ! -d ~/terminal ]; then
#     mkdir ~/terminal
# fi

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

set -e

$DOTRC/other_files/vim_update_plugins.sh
$DOTRC/other_files/generate_configs.sh
i3-msg reload
if [ "$(id -u)" != "0" ]; then
    systemctl --user enable rdm.socket
fi

if [ ! -L $XDG_CONFIG_HOME/kak/autoload/standard ]; then
    ln -s /usr/local/share/kak/autoload $XDG_CONFIG_HOME/kak/autoload/standard
fi

xmms2 server config playlist.repeat_all 1
xmms2 server config output.plugin pulse

echo -e "\nDone!"
