config_dotrc()
(
    set -e
    config_concat_dotrc_s
    if [ "$(id -u)" != "0" ]; then
        BASE_NAME=$(basename $FILE)
        systemctl --user enable $BASE_NAME
    fi
)
