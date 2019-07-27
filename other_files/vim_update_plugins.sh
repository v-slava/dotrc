#!/bin/bash

VIM_DIR=$HOME/.vim
PLUGINS_DIR=$HOME/.vim_all_plugins
VIM_CMD="$(which nvim)"
if [ -z "$VIM_CMD" ]; then
    VIM_CMD=vim
fi

set -e

if [ "$1" = "--update" ]; then
    THIS_FILE="$0"
    for DIR in $(ls $VIM_DIR/bundle/) ; do
        GIT_DIR=$VIM_DIR/bundle/$DIR
        REMOTE_URL=$(git -C $GIT_DIR config --get remote.origin.url)
        git -C $GIT_DIR checkout master
        git -C $GIT_DIR pull
        NEW_COMMIT=$(git -C $GIT_DIR rev-parse HEAD)
        cat $THIS_FILE | grep " $REMOTE_URL \\\$[^ ]\+$" | while read LINE ; do
            COMMIT_VAR="$(echo "$LINE" | cut -d' ' -f3 | cut -c 2-)"
            sed -i "$THIS_FILE" \
                -e "s/^${COMMIT_VAR}=[0-9a-f]\+$/${COMMIT_VAR}=${NEW_COMMIT}/g"
        done
    done
    exit
fi

PATHOGEN=e9fb0914dba5bdfe2feaa364dda2e9495c5620a2
# PATHOGEN=v2.4

UNIMPAIRED=5694455d72229e73ff333bfe5196cc7193dca5e7
# UNIMPAIRED=v2.0

# SWOOP=cbefdb7c17ea0eab8e1a8a1183c8f73cbb7c3611
RTAGS=3ef48de532c2e875f0fc3c33b34befed2bf37016

# YARP=8fcb1af27772174df5446d49de29052cac47e46f
LSP=dd45e31449511152f2127fe862d955237caa130f

TOML=f6f79f3cc6740dfacca73a195857cbc45e778912
WHICH_KEY=3df05b678736e7c3f744a02f0fd2958aa8121697

DENITE=7006b428e6b717793e9e323dbc51618f231e13ad
# DENITE=1.2
# For python 3.5 in file:
# ~/.vim_all_plugins/denite.nvim/rplugin/python3/denite/filter/sorter_rank.py
# use:
# from denite.filter.sorter.rank import Filter as Base

FUGITIVE=a489c6e1d3fc265242fabfd171021b0ea02dacf8
# FUGITIVE=v2.5

EASYMOTION=1a0244c90c3ff46219cf9597bb13662be4232407
# EASYMOTION=v3.0.1

VIFM=c3dc3f12ca325b6b1884db08133b02c279914813
# VIFM=v0.10

FILE_LINE=559088afaf10124ea663ee0f4f73b1de48fb1632
# FILE_LINE=1.0

TCOMMENT=ca44618f1cd7fdacadfb703954a11ac25296ac95
# TCOMMENT=3.08.1
# VIM_COMMENTARY=141d9d32a9fb58fe474fcc89cd7221eb2dd57b3a

STARTIFY=9c5680cd0b94bea9245f79463f52c7c9c6595ffd
# STARTIFY=v1.1

SURROUND=597068870b8f093a8b2d11536c62ff31222ee8d0
# SURROUND=v2.1

VIMAGIT=1.7.3

ARM_ASM_SYNTAX=0dd8d761709b2c1deb02cd44067367cc3583b084
# ARM_ASM_SYNTAX=1.2

VIM_BITBAKE=674031f0134317664d9f16ba004463b885f79cfd

MOLOKAI_COLOR_SCHEME=c67bdfcdb31415aa0ade7f8c003261700a885476

set -e

git_checkout()
{
    URL="$1"
    COMMIT="$2"
    DIR=$(basename $URL)
    GIT_DIR=$PLUGINS_DIR/$DIR
    if [ ! -d $GIT_DIR ]; then
        git -C $PLUGINS_DIR clone $URL
    fi
    git -C $GIT_DIR checkout -q $COMMIT
}

git_checkout_bundle()
{
    URL="$1"
    DIR=$(basename $URL)
    GIT_DIR=$PLUGINS_DIR/$DIR
    git_checkout "$@"
    if [ ! -e $VIM_DIR/bundle/$DIR ]; then
        ln -sr $GIT_DIR $VIM_DIR/bundle/$DIR
    fi
    if [ -d $GIT_DIR/doc ]; then
        cd $VIM_DIR/bundle/$DIR && nvim -u NONE -c "helptags doc | q" && cd -
    fi
}

mkdir -p $PLUGINS_DIR $VIM_DIR/{autoload,bundle,spell}
if [ ! -e $VIM_DIR/init.vim ]; then
    ln -sr $HOME/.vimrc $VIM_DIR/init.vim
fi

rm -f $VIM_DIR/bundle/*
git_checkout https://github.com/tpope/vim-pathogen $PATHOGEN
if [ ! -e $VIM_DIR/autoload/pathogen.vim ]; then
    ln -sr $PLUGINS_DIR/vim-pathogen/autoload/pathogen.vim $VIM_DIR/autoload/pathogen.vim
fi

git_checkout_bundle https://github.com/tpope/vim-unimpaired $UNIMPAIRED
# git_checkout_bundle https://github.com/pelodelfuego/vim-swoop $SWOOP
git_checkout_bundle https://github.com/lyuts/vim-rtags $RTAGS

# git_checkout_bundle https://github.com/roxma/nvim-yarp $YARP
git_checkout_bundle https://github.com/autozimu/LanguageClient-neovim $LSP

git_checkout_bundle https://github.com/vimscript/toml $TOML
git_checkout_bundle https://github.com/liuchengxu/vim-which-key $WHICH_KEY
git_checkout_bundle https://github.com/Shougo/denite.nvim $DENITE
git_checkout_bundle https://github.com/tpope/vim-fugitive $FUGITIVE
git_checkout_bundle https://github.com/easymotion/vim-easymotion $EASYMOTION
git_checkout_bundle https://github.com/vifm/vifm.vim $VIFM
git_checkout_bundle https://github.com/bogado/file-line $FILE_LINE

git_checkout_bundle https://github.com/tomtom/tcomment_vim $TCOMMENT
# git_checkout_bundle https://github.com/tpope/vim-commentary $VIM_COMMENTARY

git_checkout_bundle https://github.com/mhinz/vim-startify $STARTIFY
git_checkout_bundle https://github.com/tpope/vim-surround $SURROUND
git_checkout_bundle https://github.com/jreybert/vimagit $VIMAGIT

git_checkout_bundle https://github.com/dpc/vim-armasm $ARM_ASM_SYNTAX
git_checkout_bundle https://github.com/kergoth/vim-bitbake $VIM_BITBAKE
git_checkout_bundle https://github.com/tomasr/molokai $MOLOKAI_COLOR_SCHEME

if [ $TERM = "dumb" ]; then
    x-terminal-emulator -e $VIM_CMD '+UpdateRemotePlugins' '+q'
else
    nvim '+UpdateRemotePlugins' '+q'
fi
