source $DOTRC/other_files/i3_msg.sh

dotrc_s_overwrite()
(
    set -e
    if [ -f "$DOTRC_S_FILE" ]; then
        cp --preserve=mode "$DOTRC_S_FILE" "$DEST"
    else
        cp --preserve=mode "$DOTRC_FILE" "$DEST"
    fi
)

config_concat_dotrc_s()
(
    set -e
    if [ -L "$DOTRC_FILE" ]; then
        REAL_DOTRC_FILE="$(realpath "$DOTRC_FILE")"
        if [[ $REAL_DOTRC_FILE == $DOTRC/windows/* ]]; then
            DOTRC_FILE="$REAL_DOTRC_FILE"
        fi
    fi
    if [ -L "$DOTRC_FILE" ]; then
        cp -r "$DOTRC_FILE" "$DEST"
        return
    fi
    if [ -f "$DOTRC_FILE" ]; then
        cp --preserve=mode "$DOTRC_FILE" "$DEST"
        if [ -f "$DOTRC_S_FILE" ]; then
            cat "$DOTRC_S_FILE" >> "$DEST"
        fi
    # elif [ -d "$DOTRC_FILE" ]; then
    #     mkdir -p "$DEST"
    else
        cp --preserve=mode "$DOTRC_S_FILE" "$DEST"
    fi
)

config_symlink_dotrc()
(
    set -e
    rm -f "$DEST"
    ln -s "$DOTRC_FILE" "$DEST"
)

config_fix_env_vars()
(
    set -e
    ARGS=()
    while [ $# -ne 0 ]; do
        VAR_NAME="$1"
        VAR_VALUE="$(eval echo \$$VAR_NAME)"
        ARGS+=("-e" "s|${VAR_NAME}_TEMPLATE|${VAR_VALUE}|g")
        shift
    done
    if [ ${#ARGS[@]} -ne 0 ]; then
        sed -i "$DEST" "${ARGS[@]}"
    fi
)

config_virtualbox()
(
    TMP_FILE="/tmp/config_virtualbox"
    if $DOTRC/other_files/virtual_box.sh ; then
        WORD="native"
    else
        WORD="virtual"
    fi
    BEGIN=$(grep -n "# SED $WORD begin$" "$DEST" | cut -d':' -f1)
    END=$(grep -n "# SED $WORD end$" "$DEST" | cut -d':' -f1)
    head "$DEST" -n $BEGIN > "$TMP_FILE"
    tail "$DEST" -n +$END >> "$TMP_FILE"
    mv "$TMP_FILE" "$DEST"
)

config_generate_home()
(
    set -e
    case "$FILE" in
        ".spacemacs" | \
        ".bashrc")
            config_symlink_dotrc
            ;;
        ".config_xdg/i3/config")
            $DOTRC/other_files/xrandr.sh update_i3_config
            i3_msg reload
            ;;
        ".Xresources")
            config_concat_dotrc_s
            if [ -n "$DISPLAY" ]; then
                xrdb "$DEST"
            fi
            ;;
        ".config/systemd/user/rdm.socket")
            config_concat_dotrc_s
            if [ "$(id -u)" != "0" ]; then
                systemctl --user enable rdm.socket
            fi
            ;;
        ".Xmodmap")
            config_concat_dotrc_s
            config_virtualbox
            ;;
        ".config_xdg/vifm/vifmrc")
            config_concat_dotrc_s
            sed -i "$DEST" \
                -e "s|\$DOTRC_S|$DOTRC_S|g" \
                -e "s|\$DOTRC|$DOTRC|g" \
                -e "s|\$WORKSPACE|$WORKSPACE|g"
            ;;
        *) config_concat_dotrc_s
    esac
)

config_generate_root()
(
    set -e
    case "$FILE" in
        "etc/resolv.conf") dotrc_s_overwrite ;;
        *) config_concat_dotrc_s
    esac
)

config_generate()
(
    MODE="$1"
    FILE="$2"
    set -e
    case "$MODE" in
        -h)
            DOTRC_FILE="$DOTRC/home_settings/$FILE"
            DOTRC_S_FILE="$DOTRC_S/home_settings/$FILE"
            DEST_DIR="$HOME/"
            DEST="${DEST_DIR}$FILE"
            mkdir -p "$(dirname "$DEST")"
            config_generate_home
            ;;
        -r)
            DOTRC_FILE="$DOTRC/root_settings/$FILE"
            DOTRC_S_FILE="$DOTRC_S/root_settings/$FILE"
            DEST_DIR="/"
            DEST="${DEST_DIR}$FILE"
            mkdir -p "$(dirname "$DEST")"
            config_generate_root
            ;;
        *) exit 1
    esac
)
