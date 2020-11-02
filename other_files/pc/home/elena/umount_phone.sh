#!/bin/bash

DIR=/media/elena/usb
fusermount -u $DIR
RET=$?
if [ $RET -ne 0 ]; then
    zenity --error --title 'Ошибка' --text 'Не удалось выполнить отмонтирование при отключении телефона'
    exit $RET
fi
zenity --info --title 'Успех' --text 'Телефон успешно отключен, можно выдёргивать провод'
