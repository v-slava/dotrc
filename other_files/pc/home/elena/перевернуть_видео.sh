#!/bin/bash

set -e
IN="$1"
OUT="$(echo "$IN" | cut -d'.' -f-1)_перевернутое.$(echo "$IN" | cut -d'.' -f2-)"
OUT_CUT="$(echo "$OUT" | rev | cut -d'/' -f1 | rev)"
zenity --info --title 'Подождите...' --text "Видео переворачивается..." &
ZENITY_PID=$!
ffmpeg -i "$IN" -c:a copy "$OUT"
if pidof zenity | grep -q $ZENITY_PID ; then
    kill $ZENITY_PID
fi
zenity --info --title 'Успех' --text "Видео успешно перевернуто: создан новый файл $OUT_CUT"
