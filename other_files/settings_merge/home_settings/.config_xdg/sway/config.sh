config_dotrc()
(
    set -e
    config_concat_dotrc_s
    if [ "$(id -u)" -ne 0 ]; then
        swaymsg reload
    fi
)
