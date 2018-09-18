#!/bin/bash

while [ true ]; do
    thunderbird
    zenity --error --text '<span foreground="red" font="32">Do not close thunderbird window!</span>\n\n<i>It will be restarted automatically for now</i>'
done
