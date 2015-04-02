set -o vi
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -al'
alias bc='cat ~/os_settings/other_files/bc_library_my && bc -l ~/os_settings/other_files/bc_library_my'
alias glog='git log --all --graph --decorate'
# export GREP_OPTIONS=--color=auto
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
fi

# default colorless prompt:
# PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
if echo $TERM | grep -q '256color\|screen' ; then
	if [ `id -un` == root ]; then
		USER_COLOR=196
	else
		USER_COLOR=99
	fi
	PS1='\[\e[38;5;${USER_COLOR}m\]\u\[\e[38;5;178m\]@\[\e[38;5;80m\]\h \[\e[38;5;46m\]\w \[\e[38;5;27m\]\$ \[\e[0;38m\]'
else
	if [ `id -un` == root ]; then
		USER_COLOR='0;31'
	else
		USER_COLOR='0;35'
	fi
	PS1='\[\e[${USER_COLOR}m\]\u\[\e[0;33m\]@\[\e[0;36m\]\h \[\e[0;32m\]\w \[\e[0;34m\]\$ \[\e[0;37m\]'
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

if [ -z "$ORIG_PATH" ]; then
	export ORIG_PATH=$PATH
	export ORIG_PROMPT_COMMAND=$PROPMPT_COMMAND
fi

export PATH=$ORIG_PATH
export PATH=$PATH:$HOME/bin:/sbin:/usr/sbin
# Toolchains:
# Golf TV:
# export PATH=$PATH:$HOME/other/toolchains/arm-v7a15v4r3-20131011/bin/
# Tizen:
# export PATH=$PATH:$HOME/other/toolchains/armv7l-tizen-20140701/opt/cross/bin
# UHD_BD NWD:
export PATH=$PATH:$HOME/other/toolchains/arm-v7a15a7v5r2-x86_64-20140701/bin
# UHD_BD SWD:
export PATH=$PATH:$HOME/other/toolchains/build_arm_nofpu/staging_dir/bin

# Use bash's vi editing mode:
set -o vi

# Handle bash history from multiple terminals:
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it
# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $ORIG_PROMPT_COMMAND"

# Other settings:
export EDITOR=/usr/bin/vim
export MINICOM='-c on'
alias vless='vim -u /usr/share/vim/vim74/macros/less.vim'
alias cless='LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s" less -R '
alias cdiff='colordiff -uN'
export QUILT_PATCHES=debian/patches
# unset MAKEFLAGS
# export MAKEFLAGS='-j 9'

# jtag software:
# export T32SYS=/opt/t32
# export T32TMP=/tmp
# export T32ID=T32

# Bluetooth headset MAC address:
export BT_MAC='57:D3:98:0C:4A:2F'

# IP:
export IP_PC=106.125.38.23
export IP_BUILD_SERVER=106.125.52.218

# Perforce (Tizen TV):
# export P4PORT=10.41.130.122:1888
# Perforce (UHD BD):
# export P4PORT=10.41.131.67:1666
export P4PORT=pfproxy.surc.kiev.ua:1666

export P4USER=USER_NAME
# export P4PASSWD=
export TOP_DIR=/home/volkov/workspace/uhd_image/p4_workspace
export SECOS_DIR=$TOP_DIR/SECOS/SECOS

