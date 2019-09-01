config_concat_dotrc_s()
(
    FILE="$1"
    DOTRC_FILE="$DOTRC/home_settings/$FILE"
    DOTRC_S_FILE="$DOTRC_S/home_settings/$FILE"
    set -e
    DIR="$(dirname "$HOME/$FILE")"
    mkdir -p "$DIR"
    if [ -L "$DOTRC_FILE" ]; then
        cp -r "$DOTRC_FILE" "$HOME/$FILE"
        return
    fi
    cp "$DOTRC_FILE" "$HOME/$FILE"
    if [ -f "$DOTRC_S_FILE" ]; then
        cat "$DOTRC_S_FILE" >> "$HOME/$FILE"
    fi
)

config_symlink_dotrc()
(
    FILE="$1"
    set -e
    rm -f "$HOME/$FILE"
    ln -s "$DOTRC/home_settings/$FILE" "$HOME/$FILE"
)

config_fix_env_vars()
(
    FILE="$1"
    set -e
    shift
    ARGS=()
    while [ $# -ne 0 ]; do
        VAR_NAME="$1"
        VAR_VALUE="$(eval echo \$$VAR_NAME)"
        ARGS+=("-e" "s|${VAR_NAME}_TEMPLATE|${VAR_VALUE}|g")
        shift
    done
    if [ ${#ARGS[@]} -ne 0 ]; then
        sed -i "$HOME/$FILE" "${ARGS[@]}"
    fi
)

config_virtualbox()
(
    FILE="$HOME/$1"
    TMP_FILE="/tmp/config_virtualbox"
    if $DOTRC/other_files/virtual_box.sh ; then
        WORD="native"
    else
        WORD="virtual"
    fi
    BEGIN=$(grep -n "# SED $WORD begin$" "$FILE" | cut -d':' -f1)
    END=$(grep -n "# SED $WORD end$" "$FILE" | cut -d':' -f1)
    head "$FILE" -n $BEGIN > "$TMP_FILE"
    tail "$FILE" -n +$END >> "$TMP_FILE"
    mv "$TMP_FILE" "$FILE"
)

config_generate()
(
    FILE="$1"
    set -e
    case "$FILE" in
        ".spacemacs" | \
        ".bashrc")
            config_symlink_dotrc "$FILE"
            ;;
        ".config_xdg/i3/config")
            $DOTRC/other_files/xrandr.sh update_i3_config
            i3-msg reload 1>/dev/null
            ;;
        ".Xresources")
            config_concat_dotrc_s "$FILE"
            if [ -n "$DISPLAY" ]; then
                xrdb "$HOME/.Xresources"
            fi
            ;;
        ".config/systemd/user/rdm.socket")
            config_concat_dotrc_s "$FILE"
            if [ "$(id -u)" != "0" ]; then
                systemctl --user enable rdm.socket
            fi
            ;;
        ".Xmodmap")
            config_concat_dotrc_s "$FILE"
            config_virtualbox "$FILE"
            ;;
        *) config_concat_dotrc_s "$FILE"
    esac
)
