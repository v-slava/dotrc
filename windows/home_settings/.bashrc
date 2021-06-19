PATH_contains()
{
    [[ ":$PATH:" = *":$1:"* ]]
}

join_by()
{
    local IFS="$1"
    shift
    echo "$*"
}

PATH_add()
{
    MY_TMP_PATH=()
    for arg in "$@" ; do
        if ! PATH_contains "$arg" ; then
            MY_TMP_PATH+=("$arg")
        fi
    done
    MY_TMP_PATH="$(join_by : "${MY_TMP_PATH[@]}")"
}

PATH_append()
{
    PATH_add "$@"
    if [ -n "$MY_TMP_PATH" ]; then
        export PATH="$PATH:$MY_TMP_PATH"
    fi
    unset MY_TMP_PATH
}

PATH_prepend()
{
    PATH_add "$@"
    if [ -n "$MY_TMP_PATH" ]; then
        export PATH="$MY_TMP_PATH:$PATH"
    fi
    unset MY_TMP_PATH
}

if [ -z "$ORIG_PATH" ]; then
    export ORIG_PATH=$PATH
    export ORIG_PROMPT_COMMAND=$PROPMPT_COMMAND
fi

if [ -n "$WINDIR" ]; then
    # settings for windows:
    WORKSPACE=/d/root_folder/workspace
    # export EDITOR=vim
    export EDITOR=gvim
    PATH_append "/c/Program Files (x86)/Vim/vim81"
    __git_complete()
    {
        # for now we can't complete on windows...
        true
    }
else
    # settings for linux:
    WORKSPACE=/media/files/workspace
    # export EDITOR=emacs
    # export EDITOR=/usr/bin/vim
    export EDITOR=nvim
    source /usr/share/bash-completion/completions/git

    # default colorless prompt:
    # PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    if echo $TERM | grep -q '256color\|screen' ; then
        if [ `id -un` == root ]; then
            USER_COLOR=196
        else
            USER_COLOR=99
        fi
        PS1='\[\e[0;38;5;${USER_COLOR}m\]\u\[\e[38;5;178m\]@\[\e[38;5;80m\]\h \[\e[38;5;46m\]\w \[\e[38;5;27m\]\$ \[\e[0;38m\]'
    else
        if [ `id -un` == root ]; then
            USER_COLOR='0;31'
        else
            USER_COLOR='0;35'
        fi
        PS1='\[\e[0;${USER_COLOR}m\]\u\[\e[0;33m\]@\[\e[0;36m\]\h \[\e[0;32m\]\w \[\e[0;34m\]\$ \[\e[0;37m\]'
    fi

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize
fi
export DOTRC=$WORKSPACE/dotrc
export DOTRC_S=$WORKSPACE/dotrc_s

# export PATH=$ORIG_PATH
PATH_prepend $DOTRC/bin $HOME/.local/bin
PATH_prepend $HOME/.cargo/bin
PATH_append /sbin /usr/sbin

if [ -d "$DOTRC_S/bin" ]; then
    PATH_prepend $DOTRC_S/bin
fi

# XDG Base Directory Specification (canonical settings):
# export XDG_DATA_HOME=$HOME/.local/share
# export XDG_CONFIG_HOME=$HOME/.config
# export XDG_CACHE_HOME=$HOME/.cache
# XDG Base Directory Specification (my settings):
export XDG_DATA_HOME=$HOME/.data_xdg
export XDG_CONFIG_HOME=$HOME/.config_xdg
export XDG_CACHE_HOME=$HOME/.cache_xdg
# export XDG_RUNTIME_DIR=$HOME/.runtime_xdg
# XDG_RUNTIME_DIR unix access mode must be 0700, must be created on login and removed on logout.

# export CC=clang
# export CXX=clang++

# Other settings:
export MINICOM='-c on'
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgreprc
export QT_QPA_PLATFORM=wayland # use qtwayland5
# export QT_QPA_PLATFORM=xcb # use xwayland
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_TYPE=wayland

# Wayland clipboard support for root:
if [ $(id -u) -eq 0 ] && [ -f /tmp/sway_booted ]; then
    export WAYLAND_DISPLAY=wayland-0
    export XDG_RUNTIME_DIR=/run/user/1000
fi

# T909S:
# export BT_MAC=30:07:14:25:02:55
# Sony MDR-10RBT:
export BT_MAC=57:D3:98:0C:4A:2F

# iris: A new experimental Gallium driver for Intel Gen8+ GPUs
# export MESA_LOADER_DRIVER_OVERRIDE=iris
# Workaround for smplayer/vlc crash:
# https://bugs.launchpad.net/ubuntu/+source/mesa/+bug/1876219
export MESA_LOADER_DRIVER_OVERRIDE=i965

# export QUILT_PATCHES=debian/patches

# unset MAKEFLAGS
# export MAKEFLAGS='-j 9'

# jtag software:
# export T32SYS=/opt/t32
# export T32TMP=/tmp
# export T32ID=T32

# Perforce:
# export P4PORT=106.125.19.19:1666
# export P4USER=USER_NAME
# export P4PASSWD=

BASHRC_S=$DOTRC_S/home_settings/.bashrc

if [ -f $BASHRC_S ] ; then
    source $BASHRC_S
fi

set_terminal_title()
{
    echo -en "\033]0;$1\a"
}

# If not running interactively, exit now
case $- in
    *i*) ;;
      *) return;;
esac

# Append the ``new'' history lines to the history file. These are history lines
# entered since the beginning of the current bash session, but not already
# appended to the history file.
history -a

alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -al'
# export GREP_OPTIONS=--color=auto
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
# Git aliases:
alias glog='git log --graph --decorate --color=always | cless -i'
alias gloga='git log --graph --decorate --color=always --all | cless -i'
alias ga='git add'
__git_complete ga _git_add
alias gs='git status'
# alias gsh='git show HEAD'
alias gd='git diff'
__git_complete gd _git_diff
alias gco='git commit'
alias gcoa='git commit --amend'
# Git commit second:
alias gcs='$DOTRC/other_files/git_commit_dotrc_second.sh'
# Git commit update:
alias gcu='git commit --amend --no-edit -a'
__git_complete gch _git_checkout
alias gchd='git checkout --detach'
alias gb='git branch'
__git_complete gb _git_branch
alias gpl='git pull'
alias cgr='gr --color=always'
# alias gpush_test_commit='$DOTRC/other_files/git_push_test_commit.sh REPO BRANCH'
# alias gfetch_test_commit='$DOTRC/other_files/git_fetch_test_commit.sh REPO BRANCH'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ "$TERM" != "dumb" ]; then
    # disable XON/XOFF flow control for terminal (<c-s> = freeze, <c-q> = continue):
    stty -ixon

    if [ -f /usr/share/bash-completion/bash_completion ]; then
        state=$(set +o)
        set +e
        . /usr/share/bash-completion/bash_completion
        eval "$state"
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Use bash's vi editing mode:
set -o vi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
# HISTCONTROL=ignoreboth

# Handle bash history from multiple terminals:
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it
# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $ORIG_PROMPT_COMMAND"

state=$(set +o)
set +e
eval "$state"
history -c # Clear the history list by deleting all the entries.
history -r

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = "1" ] && [ "$(id -u)" != 0 ]; then
    true
    # CONFIGDIR=native
    # if [ -n "$DOTRC" ]; then
    #     source $DOTRC/other_files/config_file.sh
    #     # config_generate -h .Xmodmap
    #     # config_generate -h .config_xdg/i3/config # too early for this?
    #     if $DOTRC/other_files/virtual_box.sh ; then
    #         CONFIGDIR=virtual
    #     fi
    # fi
    # if ! dpkg -l | grep -q lightdm ; then
    # lightdm should be disabled:
    # sudo systemctl disable lightdm.service
    # exec startx -- -configdir $CONFIGDIR

    #exec startx
    exec sway

    # fi
fi
