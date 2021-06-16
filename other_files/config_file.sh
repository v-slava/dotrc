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
        echo -e "Error processing symlink: $DOTRC_FILE\n\
Please define file in \$DOTRC/other_files/settings_merge/" 1>&2
        exit 1
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

config_concat_dotrc_s_realpath()
(
    set -e
    DOTRC_FILE="$REAL_DOTRC_FILE"
    config_concat_dotrc_s
)

config_create_dir_only()
(
    set -e
    DIR="${DEST_DIR}$(realpath --relative-to "$DOTRC/home_settings" $(dirname "$DOTRC_FILE"))"
    if [ ! -d "$DIR" ]; then
        mkdir -p "$DIR"
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
    rm -rf "$DEST"
    ln -s "$DOTRC_FILE" "$DEST"
)

config_copy_realpath_dir()
(
    set -e
    if [ ! -d "$DEST" ]; then
        mkdir "$DEST"
    fi
    cp -r "$REAL_DOTRC_FILE/"* "$DEST/"
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
    REAL_DOTRC_FILE="$(realpath "$DOTRC_FILE")"
    DOTRC_S_FILE="$DOTRC_S/$SETTINGS/$FILE"
    MERGE_SUFFIX=other_files/settings_merge/$SETTINGS/${FILE}.sh
    MERGE=$DOTRC/$MERGE_SUFFIX
    MERGE_S=$DOTRC_S/$MERGE_SUFFIX
    DEST="${DEST_DIR}$FILE"
    FUNC=config_concat_dotrc_s # default
    if [ "$(basename "$FILE")" = "__keep_this_dir" ]; then
        FUNC=config_create_dir_only
    fi
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
