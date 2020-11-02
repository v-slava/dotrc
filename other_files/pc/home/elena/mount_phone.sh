#!/bin/bash

DIR=/media/elena/usb
SUBDIR='Внутренний общий накопитель'

if pidof go-mtpfs 1>/dev/null ; then
    fusermount -u $DIR
    RET=$?
    if [ $RET -ne 0 ]; then
        zenity --error --title 'Ошибка' --text 'Не удалось выполнить отмонтирование при переподключении телефона'
        exit $RET
    fi
fi
pidof go-mtpfs
if [ $? -eq 0 ]; then
    zenity --error --title 'Ошибка' --text 'Ошибка переподключения телефона'
    exit 1
fi
go-mtpfs -usb-timeout 60000 -allow-other $DIR &
sleep 0.1
EXPECTED_PID=$!
ACTUAL_PID="$(pidof go-mtpfs)"
if [ "$EXPECTED_PID" != "$ACTUAL_PID" ]; then
    zenity --error --title 'Ошибка' --text 'Не удалось подключить телефон: ошибка монтирования'
    exit $RET
fi
ITEMS="$(ls $DIR/)"
if [ "$ITEMS" != "$SUBDIR" ]; then
    zenity --error --title 'Ошибка' --text 'Не удалось подключить телефон: переведите телефон в режим передачи файлов и попробуйте снова'
    fusermount -u $DIR
    exit 1
fi
# zenity --info --title 'Успех' --text 'Телефон подключен!'
thunar "$DIR/$SUBDIR"
