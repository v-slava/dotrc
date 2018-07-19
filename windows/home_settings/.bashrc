alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -al'
# export GREP_OPTIONS=--color=auto
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
# Git aliases:
alias glog='git log --all --graph --decorate'
# alias glog='git log --all --graph --decorate --color=always | cless -i'
alias ga='git add'
__git_complete ga _git_add
alias gs='git status'
alias gsh='~/dotrc/other_files/git_show_HEAD.sh'
alias ge='~/dotrc/other_files/git_edit.sh'
alias gd='git diff'
__git_complete gd _git_diff
alias gco='git commit'
alias gcoa='git commit --amend'
# Git commit update:
alias gcu='git commit --amend --no-edit -a'
alias gch='git checkout'
__git_complete gch _git_checkout
alias gb='git branch'
__git_complete gb _git_branch
alias gpl='git pull'
set -o vi
# Handle bash history from multiple terminals:
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it
# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $ORIG_PROMPT_COMMAND"
# Other settings:
export EDITOR='"/usr/bin/vim"'
alias vless='vim -u /usr/share/vim/vim74/macros/less.vim'
alias cless='less -R'
PS1='\[\[\e[0;32m\]\w \[\e[0;34m\]\$ \[\e[0;37m\]'
alias gcs='~/dotrc/other_files/git_commit_dotrc_second.sh'
alias e='~/dotrc/bin/e'
