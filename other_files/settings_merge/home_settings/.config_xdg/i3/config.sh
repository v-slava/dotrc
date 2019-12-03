source $DOTRC/other_files/i3_msg.sh

config_dotrc()
(
    set -e
    $DOTRC/other_files/xrandr.sh update_i3_config
    i3_msg reload
)
