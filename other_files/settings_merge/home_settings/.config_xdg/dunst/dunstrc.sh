config_dotrc()
(
    set -e
    config_concat_dotrc_s
    if [ "$(id -u)" != "0" ]; then
        systemctl --user restart dunst.service
    fi
)
