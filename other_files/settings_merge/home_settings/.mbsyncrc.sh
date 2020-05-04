create_maildir()
(
    set -e
    if [ $# -ne 1 ]; then
        echo "config_create_maildir(): error: wrong args" 1>&2
        false
    fi
    DIR="$1"
    mkdir -p "$HOME/mail/isync/$DIR"
)

configure_maildirs()
(
    set -e
    create_maildir slava65536@gmail.com
    create_maildir viacheslav.volkov.1@gmail.com
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
