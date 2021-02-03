#!/usr/bin/env bash

# Extract archive to the current directory
# See also (extract heuristics): apt-cache show binwalk

set -e

PV_FLAGS=

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

    if [ -x $DOTRC_S/other_files/extract_progress.sh ]; then
        set +e
        NOT_SUPPORTED_CODE=42
        $DOTRC_S/other_files/extract_progress.sh $NOT_SUPPORTED_CODE "$FILE_FULL_PATH"
        RET=$?
        set -e
        if [ $RET -eq 0 ]; then
            continue # this file has been already successfully processed
        fi
        if [ $RET -ne $NOT_SUPPORTED_CODE ]; then
            cd -
            rm -rf "$DIR"
            exit 1 # assume error message has been already printed
        fi
    fi

    case "$FILE_FULL_PATH" in
        *.a)
            FILE_TYPE="$(file "$FILE_FULL_PATH")"
            THIN_ARCHIVE_REGEX=': thin archive with [0-9]\+ symbol entries'
            if echo "$FILE_TYPE" | grep -q "$THIN_ARCHIVE_REGEX" ; then
                ROOT_DIR="$(dirname "$FILE_FULL_PATH")"
                FILE_NAME="$(basename "$FILE_FULL_PATH")"
                set +e
                MEMBERS="$(cd "$ROOT_DIR" && ar t "$FILE_NAME")"
                RET=$?
                set -e
                if [ $RET -eq 1 ]; then
                    cat << EOF 1>&2
If you've moved this file from its original folder to some other folder then
this could be the reason of extraction failure (thin archive contains only
references to the member files of the archive). Try to extract this archive in
its original folder instead.
EOF
                fi
                if [ $RET -ne 0 ]; then
                    exit $RET
                fi
                readarray -t members <<< "$MEMBERS"
                for f in "${members[@]}" ; do
                    d="$(dirname "$f")"
                    mkdir -p "$d"
                    ln -s "$ROOT_DIR/$f" "$f"
                done
            else
                ar x "$FILE_FULL_PATH"
            fi
            ;;
        *.deb | *.ipk) ar x "$FILE_FULL_PATH" ;;
        *.rpm) rpm2cpio "$FILE_FULL_PATH" | pv $PV_FLAGS | cpio -idm ;;
        *.sh)
            split_script_binary_archive.sh -ob data.bin "$FILE_FULL_PATH"
            MIME_TYPE=$(file -b --mime-type data.bin)
            if [ "$MIME_TYPE" = "application/x-xz" ]; then
                mv data.bin data.xz
                $0 data.xz
                cd extracted_data.xz
                MIME_TYPE=$(file -b --mime-type data)
                if [ "$MIME_TYPE" = "application/x-tar" ]; then
                    mv data data.tar
                    $0 data.tar
                fi
            fi
            ;;
        *.jar)              pv $PV_FLAGS "$FILE_FULL_PATH" | jar x         ;;
        *.tar)              pv $PV_FLAGS "$FILE_FULL_PATH" | tar x         ;;
        *.tgz | *.tar.gz)   pv $PV_FLAGS "$FILE_FULL_PATH" | tar xz        ;;
        *.tar.lz)           pv $PV_FLAGS "$FILE_FULL_PATH" | tar x --lzip  ;;
        *.tar.lzma)         pv $PV_FLAGS "$FILE_FULL_PATH" | tar x --lzma  ;;
        *.tar.zst)          pv $PV_FLAGS "$FILE_FULL_PATH" | tar x -I zstd ;;
        *.tar.bz2 | *.tbz2) pv $PV_FLAGS "$FILE_FULL_PATH" | tar xj        ;;
        *.tar.xz | *.txz)   pv $PV_FLAGS "$FILE_FULL_PATH" | tar xJ        ;;
        *.xz)
            UNCOMPRESSED_FILE="`echo $FILE_FULL_PATH | rev | cut -d'/' -f1 | cut -d'.' -f 2- | rev`"
            pv $PV_FLAGS "$FILE_FULL_PATH" | unxz > "$UNCOMPRESSED_FILE" ;;
        *.gz)
            UNCOMPRESSED_FILE="`echo $FILE_FULL_PATH | rev | cut -d'/' -f1 | cut -d'.' -f 2- | rev`"
            pv $PV_FLAGS "$FILE_FULL_PATH" | gunzip > "$UNCOMPRESSED_FILE" ;;
        *.bz2)
            UNCOMPRESSED_FILE="`echo $FILE_FULL_PATH | rev | cut -d'/' -f1 | cut -d'.' -f 2- | rev`"
            pv $PV_FLAGS "$FILE_FULL_PATH" | bunzip2 "$UNCOMPRESSED_FILE" ;;
        *.igz)
            # pv $PV_FLAGS "$FILE_FULL_PATH" | gunzip -c | cpio -i -d -H newc --no-absolute-filenames ;;
            unmkinitramfs "$FILE_FULL_PATH" . ;;
        *.squashfs) unsquashfs -d rootfs "$FILE_FULL_PATH" ;;
        *.cpio) pv $PV_FLAGS "$FILE_FULL_PATH" | cpio -i ;;
        # *.7z) pv $PV_FLAGS "$FILE_FULL_PATH" | 7z x -si > "$UNCOMPRESSED_FILE" ;; # Error: E_NOTIMPL
        *.7z) 7z x "$FILE_FULL_PATH" ;;
        *.rar) rar x "$FILE_FULL_PATH" ;; # impossible to read from stdin?
        *.zip | *.apk) unzip "$FILE_FULL_PATH" ;; # impossible to read from stdin?
        # *.Z) uncompress "$FILE_FULL_PATH" ;;
        *)
            FILE_TYPE="$(file --brief "$FILE_FULL_PATH")"
            case "$FILE_TYPE" in
                u-boot\ legacy\ uImage,\ *Multi-File\ Image\ \(Not\ compressed\),\ *)
                    METAINF=meta_information.txt
                    echo "$ dumpimage -l $FILE" > $METAINF
                    OUTPUT="$(dumpimage -l "$FILE_FULL_PATH")"
                    echo -e "$OUTPUT" >> $METAINF
                    # echo -e "$OUTPUT" | grep '   Image .:' | cut -c 5-
                    mkdir images
                    NUM_IMAGES=$(echo -e "$OUTPUT" | tail -n1 | cut -c 10- | cut -d: -f1)
                    for i in $(seq 0 ${NUM_IMAGES}) ; do
                        dumpimage -p $i -o images/$i "$FILE_FULL_PATH"
                    done
                    ;;
                *)
                    echo "Error: \"$FILE\" - unknown archive format." 1>&2
                    # see also: https://github.com/robbyrussell/oh-my-zsh/tree/master/plugins/extract
                    cd -
                    rm -rf "$DIR"
                    exit 1
                    ;;
            esac
            ;;
    esac
done

