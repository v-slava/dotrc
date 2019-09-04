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
find ! -type d | cut -d'/' -f 2- | while read FILE ; do
    config_generate "$FILE"
done

$DOTRC/other_files/vim_update_plugins.sh

if [ "$(id -u)" = "0" ]; then
    echo "Done for user: $(whoami)!"
    exit 0
fi

MEDIA_FILES=/media/files

if [ ! -L ~/my ]; then
    ln -s $MEDIA_FILES/temporary/my ~/my
fi
if [ ! -L ~/bin ]; then
    ln -s $DOTRC/bin ~/bin
fi
if [ ! -L ~/downloads ]; then
    ln -s $MEDIA_FILES/downloads ~/downloads
fi
if [ ! -L ~/Downloads ]; then
    ln -s $MEDIA_FILES/downloads ~/Downloads
fi
if [ ! -L ~/Desktop ]; then
    ln -s $MEDIA_FILES/downloads ~/Desktop
fi
# if [ ! -d ~/terminal ]; then
#     mkdir ~/terminal
# fi

if [ "$(id -u)" != "0" ]; then
    mkdir -p $MEDIA_FILES/{downloads,temporary/my,workspace,other}
fi

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

if [ ! -L $XDG_CONFIG_HOME/kak/autoload/standard ]; then
    ln -s /usr/local/share/kak/autoload $XDG_CONFIG_HOME/kak/autoload/standard
fi

xmms2 server config playlist.repeat_all 1
xmms2 server config output.plugin pulse

echo "Done for user: $(whoami)!"
