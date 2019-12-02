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
        cp -P "$DOTRC_FILE" "$DEST"
        return
    fi
    if [ -f "$DOTRC_FILE" ]; then
        cp --preserve=mode "$DOTRC_FILE" "$DEST"
        if [ -f "$DOTRC_S_FILE" ]; then
            cat "$DOTRC_S_FILE" >> "$DEST"
        fi
    else
        cp --preserve=mode "$DOTRC_S_FILE" "$DEST"
    fi
)

config_preprocess()
(
    set -e
    if [ ! -f "$DOTRC_FILE" ]; then
        echo "Error: $DOTRC_FILE is not a regular file" 1>&2
        return 1
    fi
    rm -rf "$DEST"
    REGEX='.*#INCLUDE_FILE_FROM_DOTRC_S: (\S+)'
    cat "$DOTRC_FILE" | $DOTRC/other_files/preprocess.py "$REGEX" \
        "$DOTRC_S/other_files/$SETTINGS" > "$DEST"
)

config_copy_symlink()
(
    set -e
    rm -rf "$DEST"
    cp -P "$DOTRC_FILE" "$DEST"
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
        ".mbsyncrc")
            if ! $DOTRC/other_files/virtual_box.sh ; then
                config_preprocess
                chmod 600 "$DEST"
                if [ "$(id -u)" != "0" ]; then
                    mkdir -p $HOME/mail/slava65536@gmail.com
                    mkdir -p $HOME/mail/viacheslav.volkov.1@gmail.com
                    # Add short symlinks for sidebar in neomutt:
                    if [ ! -L $HOME/mail/s ]; then
                        ln -s $HOME/mail/slava65536@gmail.com $HOME/mail/s
                    fi
                    if [ ! -L $HOME/mail/v ]; then
                        ln -s $HOME/mail/viacheslav.volkov.1@gmail.com \
                            $HOME/mail/v
                    fi
                fi
            fi
            ;;
        ".msmtprc")
            if ! $DOTRC/other_files/virtual_box.sh ; then
                config_preprocess
                chmod 600 "$DEST"
            fi
            ;;
        ".spacemacs" | \
        ".bashrc")
            config_symlink_dotrc
            ;;
        ".Xmodmap")
            config_concat_dotrc_s
            config_virtualbox
            ;;
        ".Xresources")
            config_concat_dotrc_s
            if [ -n "$DISPLAY" ]; then
                xrdb "$DEST"
            fi
            ;;
        ".config_xdg/i3/config")
            $DOTRC/other_files/xrandr.sh update_i3_config
            i3_msg reload
            ;;
        ".config_xdg/vifm/vifmrc")
            config_concat_dotrc_s
            sed -i "$DEST" \
                -e "s|\$DOTRC_S|$DOTRC_S|g" \
                -e "s|\$DOTRC|$DOTRC|g" \
                -e "s|\$WORKSPACE|$WORKSPACE|g"
            ;;
        ".config/dunst/dunstrc")
            config_concat_dotrc_s
            if [ "$(id -u)" != "0" ]; then
                systemctl --user restart dunst.service
            fi
            ;;
        ".config/systemd/user/mbsync.timer" | \
        ".config/systemd/user/rdm.socket")
            config_concat_dotrc_s
            if [ "$(id -u)" != "0" ]; then
                BASE_NAME=$(basename $FILE)
                systemctl --user enable $BASE_NAME
            fi
            ;;
        ".vim/init.vim")
            config_copy_symlink
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
            SETTINGS="home_settings"
            DEST_DIR="$HOME/"
            FUNC=config_generate_home
            ;;
        -r)
            SETTINGS="root_settings"
            DEST_DIR="/"
            FUNC=config_generate_root
            ;;
        *) exit 1
    esac
    DOTRC_FILE="$DOTRC/$SETTINGS/$FILE"
    DOTRC_S_FILE="$DOTRC_S/$SETTINGS/$FILE"
    DEST="${DEST_DIR}$FILE"
    mkdir -p "$(dirname "$DEST")"
    $FUNC
)
