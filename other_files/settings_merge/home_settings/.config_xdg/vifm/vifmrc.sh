config_dotrc()
(
    set -e
    config_concat_dotrc_s
    sed -i "$DEST" \
        -e "s|\$DOTRC_S|$DOTRC_S|g" \
        -e "s|\$DOTRC|$DOTRC|g" \
        -e "s|\$WORKSPACE|$WORKSPACE|g"
)
