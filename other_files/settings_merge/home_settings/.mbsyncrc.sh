config_dotrc()
(
    set -e
    if ! $DOTRC/other_files/virtual_box.sh ; then
        config_preprocess
        chmod 600 "$DEST"
        if [ "$(id -u)" != "0" ]; then
            mkdir -p $HOME/mail/slava65536@gmail.com
            mkdir -p $HOME/mail/viacheslav.volkov.1@gmail.com
            # Add short symlinks for sidebar in neomutt:
            if [ ! -L $HOME/mail/s ]; then
                ln -s $HOME/mail/slava65536@gmail.com $HOME/mail/s
            fi
            if [ ! -L $HOME/mail/v ]; then
                ln -s $HOME/mail/viacheslav.volkov.1@gmail.com \
                    $HOME/mail/v
            fi
        fi
    fi
)
