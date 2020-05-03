create_maildir()
(
    set -e
    if [ $# -ne 2 ]; then
        echo "config_create_maildir(): error: wrong args" 1>&2
        false
    fi
    LONG="$1"
    SHORT="$2"
    mkdir -p $HOME/mail/$LONG
    # Add short symlink for sidebar in mutt:
    if [ ! -L $HOME/mail/$SHORT ]; then
        ln -s $HOME/mail/$LONG $HOME/mail/$SHORT
    fi
)

configure_maildirs()
(
    set -e
    create_maildir slava65536@gmail.com s
    create_maildir viacheslav.volkov.1@gmail.com v
)

config_dotrc()
(
    set -e
    # if ! $DOTRC/other_files/virtual_box.sh ; then
    if [ "$(id -u)" != "0" ]; then
        config_preprocess
        chmod 600 "$DEST"
        configure_maildirs
    fi
    # fi
)
