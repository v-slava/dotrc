#!/usr/bin/env bash

# Extract archive to the current directory

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

	echo "Extracting \"$FILE\"..."
	case "$FILE" in
		*.a) ar x "$FILE" ;;
		*.tar)				pv "$FILE" | tar x ;;
		*.tgz | *.tar.gz)	pv "$FILE" | tar xz ;;
		*.tar.bz2 | *.tbz2)	pv "$FILE" | tar xj ;;
		*.tar.xz | *.txz)	pv "$FILE" | tar xJ ;;
		*.gz)
			UNCOMPRESSED_FILE="`echo $FILE | rev | cut -d'.' -f 2- | rev`"
			pv "$FILE" | gunzip > "$UNCOMPRESSED_FILE" ;;
		*.bz2)
			UNCOMPRESSED_FILE="`echo $FILE | rev | cut -d'.' -f 2- | rev`"
			pv "$FILE" | bunzip2 "$UNCOMPRESSED_FILE" ;;
		*.igz)
			DIR="${FILE}_extracted"
			if [ -e "$DIR" ]; then
				echo "Error: cannot extract: \"$DIR\" exists." 1>&2
				exit 1
			fi
			FILE_FULL_PATH=`readlink -f "$FILE"`
			mkdir "$DIR" && cd "$DIR" && pv "$FILE_FULL_PATH" | gunzip -c | \
				cpio -i -d -H newc --no-absolute-filenames
		;;
		# *.7z) pv "$FILE" | 7z x -si > "$UNCOMPRESSED_FILE" ;; # Error: E_NOTIMPL
		*.7z) 7z x "$FILE" ;;
		*.rar) rar x "$FILE" ;; # impossible to read from stdin?
		*.zip) unzip "$FILE" ;; # impossible to read from stdin?
		# *.Z) uncompress "$FILE" ;;
		*)
			echo "Error: \"$FILE\" - unknown archive format." 1>&2
			exit 1
		;;
	esac
done

