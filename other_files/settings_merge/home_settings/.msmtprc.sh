config_dotrc()
(
    set -e
    if ! $DOTRC/other_files/virtual_box.sh ; then
        config_preprocess
        chmod 600 "$DEST"
    fi
)
