export WORKSPACE=/media/files/workspace
export DOTRC=$WORKSPACE/dotrc
export DOTRC_S=$WORKSPACE/dotrc_s

export PI_PORT=53535
export PI_USR=pi
export PI_HOST=94.154.220.9
export PI_SSH="ssh -p $PI_PORT $PI_USR@$PI_HOST"

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

if [ -z "$ORIG_PATH" ]; then
	export ORIG_PATH=$PATH
	export ORIG_PROMPT_COMMAND=$PROPMPT_COMMAND
fi

export PATH=$ORIG_PATH
export PATH=$HOME/bin:$HOME/.local/bin:$PATH:/sbin:/usr/sbin
export CC=clang-6.0
export CXX=clang++-6.0

# Other settings:
export EDITOR=/usr/bin/vim
export MINICOM='-c on'

export QUILT_PATCHES=debian/patches
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
alias mbc='cat $DOTRC/other_files/bc_library_my && bc -l $DOTRC/other_files/bc_library_my'
# export GREP_OPTIONS=--color=auto
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
# Git aliases:
source /usr/share/bash-completion/completions/git
alias glog='git log --graph --decorate --color=always | cless -i'
alias gloga='git log --graph --decorate --color=always --all | cless -i'
alias ga='git add'
__git_complete ga _git_add
alias gs='git status'
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
# alias gpush_test_commit='$DOTRC/other_files/git_push_test_commit.sh REPO BRANCH'
# alias gfetch_test_commit='$DOTRC/other_files/git_fetch_test_commit.sh REPO BRANCH'
alias beautify='~/workspace/dotrc_s/constant_scripts/beautify.sh'

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

# Use bash's vi editing mode:
set -o vi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

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
