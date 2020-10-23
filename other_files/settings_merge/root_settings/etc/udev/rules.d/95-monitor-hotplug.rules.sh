config_dotrc()
(
    set -e
    if ! diff "$DOTRC_FILE" "$DEST" 1>/dev/null 2>&1 ; then
        cp --preserve=mode "$DOTRC_FILE" "$DEST"
        echo "Reloading udev rules..."
        udevadm control --reload
    fi
)
