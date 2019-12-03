config_dotrc()
(
    set -e
    config_concat_dotrc_s
    if [ -n "$DISPLAY" ]; then
        xrdb "$DEST"
    fi
)
