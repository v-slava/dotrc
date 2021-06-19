#!/bin/bash

# WATERMARK_SH=$DOTRC/other_files/my_samples/single_sources/watermark_images.sh
# mkdir in && (cd in && pdfimages -all ../input.pdf file)
# mkdir out && $WATERMARK_SH in out 130 "Виключно для ідентифікації\nособистості компанією XXXXX"
# convert out/file* output.pdf

usage()
{
    echo "Usage: $(basename $0) IN_DIR OUT_DIR TEXT_SIZE TEXT" 1>&2
    exit 1
}

if [ $# -ne 4 ]; then
    usage
fi

IN_DIR="$1"
if [ ! -d "$IN_DIR" ]; then
    usage
fi
shift
OUT_DIR="$1"
if [ ! -d "$OUT_DIR" ]; then
    usage
fi
shift
TEXT_SIZE="$1"
shift
TEXT="$1"
shift

set -e
FILES_LIST="$(ls -a "$IN_DIR" | grep -v '^\.\.\?$')"

MIN_WIDTH=99999999999
MIN_HEIGHT=9999999999
for IN_FILE in $FILES_LIST ; do
    WIDTH=$(identify -format %w "$IN_DIR/$IN_FILE")
    HEIGHT=$(identify -format %h "$IN_DIR/$IN_FILE")
    if [ $WIDTH -lt $MIN_WIDTH ]; then
        MIN_WIDTH=$WIDTH
    fi
    if [ $HEIGHT -lt $MIN_HEIGHT ]; then
        MIN_HEIGHT=$HEIGHT
    fi
done

ANGLE=$((45 * $MIN_HEIGHT / $MIN_WIDTH))

# IMAGE_WIDTH=$MIN_WIDTH
# IMAGE_HEIGHT=$MIN_HEIGHT
IMAGE_WIDTH=5000
IMAGE_HEIGHT=5000
IMAGE_SIZE=${IMAGE_WIDTH}x${IMAGE_HEIGHT}
FONT=/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf
WATERMARK_IMAGE=/tmp/my_watermark.miff

rm -f "$WATERMARK_IMAGE"

    # -fill grey \
convert \
    -font "$FONT" \
    -size $IMAGE_SIZE \
    xc:transparent \
    -fill rgba\(50,50,50,150\) \
    -gravity center \
    -pointsize $TEXT_SIZE \
    -draw "text 0,0 '$TEXT'" \
    -rotate $ANGLE -extent $IMAGE_SIZE \
    miff:- > "$WATERMARK_IMAGE"
    # miff:- | composite -gravity center - input.jpg output.jpg

for IN_FILE in $FILES_LIST ; do
    composite -gravity center "$WATERMARK_IMAGE" "$IN_DIR/$IN_FILE" \
        "$OUT_DIR/$IN_FILE"
done
rm "$WATERMARK_IMAGE"
