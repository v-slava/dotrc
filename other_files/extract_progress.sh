#!/usr/bin/env bash

# Extract archive to the current directory

set -e

usage()
{
    echo "Usage: $(basename $0) FILE ..." 1>&2
    exit 1
}

if [ $# -lt 1 ]; then
    usage
fi

for FILE in "$@" ; do
    if [ ! -f "$FILE" ]; then
        echo "Error: \"$FILE\" is not a valid archive file." 1>&2
        usage
    fi

    FILE_FULL_PATH="$(readlink -f "$FILE")"
    DIR="extracted_${FILE}"
    if [ -e "$DIR" ]; then
        echo "Error: cannot extract: \"$DIR\" exists." 1>&2
        exit 1
    fi
    mkdir "$DIR"
    cd "$DIR"
    echo "Extracting \"$FILE\" to \"$DIR\"..."

    case "$FILE_FULL_PATH" in
        *.a | *.deb) ar x "$FILE_FULL_PATH" ;;
        *.jar) pv "$FILE_FULL_PATH" | jar x ;;
        *.tar)                pv "$FILE_FULL_PATH" | tar x ;;
        *.tgz | *.tar.gz)    pv "$FILE_FULL_PATH" | tar xz ;;
        *.tar.bz2 | *.tbz2)    pv "$FILE_FULL_PATH" | tar xj ;;
        *.tar.xz | *.txz)    pv "$FILE_FULL_PATH" | tar xJ ;;
        *.gz)
            UNCOMPRESSED_FILE="`echo $FILE_FULL_PATH | rev | cut -d'/' -f1 | cut -d'.' -f 2- | rev`"
            pv "$FILE_FULL_PATH" | gunzip > "$UNCOMPRESSED_FILE" ;;
        *.bz2)
            UNCOMPRESSED_FILE="`echo $FILE_FULL_PATH | rev | cut -d'/' -f1 | cut -d'.' -f 2- | rev`"
            pv "$FILE_FULL_PATH" | bunzip2 "$UNCOMPRESSED_FILE" ;;
        *.igz)
            pv "$FILE_FULL_PATH" | gunzip -c | cpio -i -d -H newc --no-absolute-filenames ;;
        # *.7z) pv "$FILE_FULL_PATH" | 7z x -si > "$UNCOMPRESSED_FILE" ;; # Error: E_NOTIMPL
        *.7z) 7z x "$FILE_FULL_PATH" ;;
        *.rar) rar x "$FILE_FULL_PATH" ;; # impossible to read from stdin?
        *.zip | *.apk) unzip "$FILE_FULL_PATH" ;; # impossible to read from stdin?
        # *.Z) uncompress "$FILE_FULL_PATH" ;;
        *)
            echo "Error: \"$FILE\" - unknown archive format." 1>&2
            exit 1
        ;;
    esac
done

