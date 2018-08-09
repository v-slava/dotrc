DOTRC_PATH=$DOTRC/home_settings
DOTRC_S_PATH=$DOTRC_S/home_settings
DOTRC_FILE="$DOTRC_PATH/$FILE"
DOTRC_S_FILE="$DOTRC_S_PATH/$FILE"
if [ ! -f "$DOTRC_FILE" ]; then
    echo "File not found: $DOTRC_FILE" 1>&2
    exit 2
fi
