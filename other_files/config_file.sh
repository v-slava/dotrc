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
    IN_FILE="$DOTRC_FILE"
    INC_SUFFIX=other_files/settings_merge/preprocess_include
    if [ -f "$DOTRC_S_FILE" ]; then
        IN_FILE="$DOTRC_S_FILE"
    fi
    cat "$IN_FILE" | $DOTRC/other_files/preprocess.py \
        --include-regex '.*#INCLUDE_FILE: (\S+)' \
        --define-regex '.*#DEFINE (\w+) (.+)\n' \
        --undef-regex '.*#UNDEF (\w+)' \
        --included-lines \
            "#DEFINE DOTRC_S $DOTRC_S" \
            "#DEFINE DOTRC $DOTRC" \
        --allow-redefine --include-dirs \
            "$DOTRC_S/$INC_SUFFIX" \
            "$DOTRC/$INC_SUFFIX" \
        > "$DEST"
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

config_generate()
(
    MODE="$1"
    FILE="$2"
    set -e
    case "$MODE" in
        -h)
            SETTINGS="home_settings"
            DEST_DIR="$HOME/"
            ;;
        -r)
            SETTINGS="root_settings"
            DEST_DIR="/"
            ;;
        *) exit 1
    esac
    DOTRC_FILE="$DOTRC/$SETTINGS/$FILE"
    DOTRC_S_FILE="$DOTRC_S/$SETTINGS/$FILE"
    MERGE_SUFFIX=other_files/settings_merge/$SETTINGS/${FILE}.sh
    MERGE=$DOTRC/$MERGE_SUFFIX
    MERGE_S=$DOTRC_S/$MERGE_SUFFIX
    DEST="${DEST_DIR}$FILE"
    FUNC=config_concat_dotrc_s # default
    if [ -f "$MERGE" ]; then
        . "$MERGE"
        FUNC=config_dotrc
    fi
    if [ -f "$MERGE_S" ]; then
        . "$MERGE_S"
        FUNC=config_dotrc_s
    fi
    mkdir -p "$(dirname "$DEST")"
    $FUNC
)
