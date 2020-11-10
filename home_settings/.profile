# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [ ! true ] ; then

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

else # if [ ! true ]
    source "$HOME/.bashrc"
fi

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = "1" ]; then
    CONFIGDIR=native
    if [ -n "$DOTRC" ]; then
        source $DOTRC/other_files/config_file.sh
        config_generate -h .Xmodmap
        config_generate -h .config_xdg/i3/config
        if $DOTRC/other_files/virtual_box.sh ; then
            CONFIGDIR=virtual
        fi
    fi
    # if ! dpkg -l | grep -q lightdm ; then
    # lightdm should be disabled:
    # sudo systemctl disable lightdm.service
    # exec startx -- -configdir $CONFIGDIR
    exec startx
    # fi
fi
