#!/bin/bash

VIM_DIR=$HOME/.vim
PLUGINS_DIR=$HOME/.vim_all_plugins

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

UNIMPAIRED=a49c4f2bf05f18a6e4f6572a19763ba7abba52b1
# UNIMPAIRED=v2.0

# SWOOP=cbefdb7c17ea0eab8e1a8a1183c8f73cbb7c3611
RTAGS=3ef48de532c2e875f0fc3c33b34befed2bf37016

# YARP=8fcb1af27772174df5446d49de29052cac47e46f
# LSP=master
# LSP=0.1.147
# LSP_BRANCH=next
# COC_NVIM=6d4508c3660373331383848e15820a8c940d028f # requires node.js!
# https://github.com/w0rp/ale
# https://github.com/prabirshrestha/vim-lsp
# https://github.com/natebosch/vim-lsc
# https://github.com/m-pilia/vim-ccls

TOML=f6f79f3cc6740dfacca73a195857cbc45e778912
WHICH_KEY=80a1e88f1df5b8b0a203dd9973dd164192bb67cf

DENITE=2.1
# DENITE=1.2
# For python 3.5 in file:
# ~/.vim_all_plugins/denite.nvim/rplugin/python3/denite/filter/sorter_rank.py
# use:
# from denite.filter.sorter.rank import Filter as Base

FUGITIVE=881ad1ed0bd88acc7568c07e35daa4b81c4aa1c9
# FUGITIVE=v2.5

EASYMOTION=85e90c9759e14633d878ed534ef313876ab96555
# EASYMOTION=v3.0.1

VIFM=1714b319f459a6e705999902329c7ef1a30d108f
# VIFM=v0.10

# FILE_LINE=559088afaf10124ea663ee0f4f73b1de48fb1632
# FILE_LINE=1.0
VIM_FETCH=76c08586e15e42055c9c21321d9fca0677442ecc

TCOMMENT=9de9f7611297a1198b782d81eca84ec49e86008b
# TCOMMENT=3.08.1
# VIM_COMMENTARY=141d9d32a9fb58fe474fcc89cd7221eb2dd57b3a

STARTIFY=a27df8325c9ae7e4b0aa677936137dbf94e73b42
# STARTIFY=v1.1

SURROUND=fab8621670f71637e9960003af28365129b1dfd0
# SURROUND=v2.1

VIMAGIT=1.7.3

DETECTINDENT=c09ab6effc29ef913c531816eb980460f8dc9ed2

ARM_ASM_SYNTAX=0dd8d761709b2c1deb02cd44067367cc3583b084
# ARM_ASM_SYNTAX=1.2

VIM_BITBAKE=674031f0134317664d9f16ba004463b885f79cfd

MOLOKAI_COLOR_SCHEME=c67bdfcdb31415aa0ade7f8c003261700a885476
# ANSI_ESC=12
# ANSI_ESC_2=690f820d20b6e3a79ba20499874eb7333aa4ca5c

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
    if ! git -C $GIT_DIR checkout -q $COMMIT 2>/dev/null ; then
        git -C $GIT_DIR fetch origin
        git -C $GIT_DIR checkout -q $COMMIT
    fi
}

git_checkout_bundle()
(
    URL="$1"
    DIR=$(basename $URL)
    GIT_DIR=$PLUGINS_DIR/$DIR
    set -e
    git_checkout "$@"
    if [ ! -e $VIM_DIR/bundle/$DIR ]; then
        ln -sr $GIT_DIR $VIM_DIR/bundle/$DIR
    fi
    # This causes too much screen blinking:
    # if [ -d $GIT_DIR/doc ]; then
    #     cd $VIM_DIR/bundle/$DIR
    #     $VIM_CMD -u NONE -c "helptags doc | q"
    #     cd - 1>/dev/null
    # fi
)

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
# git_checkout_bundle https://github.com/autozimu/LanguageClient-neovim $LSP
# git_checkout_bundle https://github.com/neoclide/coc.nvim $COC_NVIM

git_checkout_bundle https://github.com/vimscript/toml $TOML
git_checkout_bundle https://github.com/liuchengxu/vim-which-key $WHICH_KEY
git_checkout_bundle https://github.com/Shougo/denite.nvim $DENITE
git_checkout_bundle https://github.com/tpope/vim-fugitive $FUGITIVE
git_checkout_bundle https://github.com/easymotion/vim-easymotion $EASYMOTION
git_checkout_bundle https://github.com/vifm/vifm.vim $VIFM

# git_checkout_bundle https://github.com/bogado/file-line $FILE_LINE
git_checkout_bundle https://github.com/wsdjeg/vim-fetch $VIM_FETCH

git_checkout_bundle https://github.com/tomtom/tcomment_vim $TCOMMENT
# git_checkout_bundle https://github.com/tpope/vim-commentary $VIM_COMMENTARY

git_checkout_bundle https://github.com/mhinz/vim-startify $STARTIFY
git_checkout_bundle https://github.com/tpope/vim-surround $SURROUND
git_checkout_bundle https://github.com/jreybert/vimagit $VIMAGIT
git_checkout_bundle https://github.com/ciaranm/detectindent $DETECTINDENT

git_checkout_bundle https://github.com/dpc/vim-armasm $ARM_ASM_SYNTAX
git_checkout_bundle https://github.com/kergoth/vim-bitbake $VIM_BITBAKE
git_checkout_bundle https://github.com/tomasr/molokai $MOLOKAI_COLOR_SCHEME
# git_checkout_bundle https://github.com/vim-scripts/AnsiEsc.vim $ANSI_ESC
# git_checkout_bundle https://github.com/powerman/vim-plugin-AnsiEsc $ANSI_ESC_2

cd $HOME/.vim/bundle
ARGS=('+UpdateRemotePlugins')
ls | while read dir ; do
    ARGS+=("+helptags $dir/doc")
done
ARGS+=('+q')
if [ $TERM = "dumb" ]; then
    x-terminal-emulator -e nvim "${ARGS[@]}"
else
    nvim "${ARGS[@]}"
fi
