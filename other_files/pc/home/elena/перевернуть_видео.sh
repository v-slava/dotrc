#!/bin/bash

set -e
IN="$1"
OUT="$(echo "$IN" | cut -d'.' -f-1)_перевернутое.$(echo "$IN" | cut -d'.' -f2-)"
echo "|$PWD|$IN|$OUT|" >> /tmp/rotate_video.sh
OUT_CUT="$(echo "$OUT" | rev | cut -d'/' -f1 | rev)"
zenity --info --title 'Подождите...' --text "Видео переворачивается..." &
ffmpeg -i "$IN" -c:a copy "$OUT"
set +e
killall zenity
set -e
zenity --info --title 'Успех' --text "Видео успешно перевернуто: создан новый файл $OUT_CUT"
