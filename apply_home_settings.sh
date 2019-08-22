#!/usr/bin/env bash

if [ -n "$WINDIR" ]; then
    echo -e "This script is not applicable for windows." 1>&2
    read -p "Press enter to continue..."
    exit 1
fi

set -e

if [ -z "$DOTRC" ]; then
    echo "Error: \$DOTRC is not defined" 1>&2
    exit 1
fi
cd $DOTRC/home_settings
source $DOTRC/other_files/config_file.sh
find -type f | cut -d'/' -f 2- | while read FILE ; do
    config_generate "$FILE"
done

if [ "$(id -u)" = "0" ]; then
    echo "Done for user: $(whoami)!"
    exit 0
fi

MEDIA_FILES=/media/files

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

$DOTRC/other_files/vim_update_plugins.sh

if [ ! -L $XDG_CONFIG_HOME/kak/autoload/standard ]; then
    ln -s /usr/local/share/kak/autoload $XDG_CONFIG_HOME/kak/autoload/standard
fi

xmms2 server config playlist.repeat_all 1
xmms2 server config output.plugin pulse

echo "Done for user: $(whoami)!"
