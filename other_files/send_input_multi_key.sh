#!/bin/bash

# See also ~/.XCompose
FILE=/tmp/dotrc_send_input_multi_key_output
rm -f $FILE

if [ $# -eq 0 ]; then
    $DOTRC/other_files/open_terminal.sh --title 'Multi_key script' $DOTRC/other_files/send_input_multi_key.sh --execute
    if [ -f "$FILE" ]; then
        cat "$FILE" | xargs wl-copy
    fi
    exit
fi


LAYOUT=$($DOTRC/other_files/keyboard_layout.sh --get)
if [ "$LAYOUT" = "US" ]; then
    cat << EOF
<Multi_key> <4> : "§"
<Multi_key> <e> : "€"
<Multi_key> <m> : "μ"

<Multi_key> <a> : "ä"           <Multi_key> <A> : "Ä"
<Multi_key> <u> : "ü"           <Multi_key> <U> : "Ü"
<Multi_key> <o> : "ö"           <Multi_key> <O> : "Ö"
<Multi_key> <s> : "ß"           <Multi_key> <S> : "ẞ"

Select what to type (press single character). Your choice:
EOF
    read -n1 INPUT_SYMBOL
    case "$INPUT_SYMBOL" in
        "4") OUTPUT_SYMBOL="§" ;;
        "e") OUTPUT_SYMBOL="€" ;;
        "m") OUTPUT_SYMBOL="μ" ;;

        "a") OUTPUT_SYMBOL="ä" ;;
        "A") OUTPUT_SYMBOL="Ä" ;;
        "o") OUTPUT_SYMBOL="ö" ;;
        "O") OUTPUT_SYMBOL="Ö" ;;
        "u") OUTPUT_SYMBOL="ü" ;;
        "U") OUTPUT_SYMBOL="Ü" ;;
        "s") OUTPUT_SYMBOL="ß" ;;
        "S") OUTPUT_SYMBOL="ẞ" ;;
        *) OUTPUT_SYMBOL="" ;;
    esac
else
    if [ "$LAYOUT" != "RU" ]; then
        echo "Bug in $(realpath $0)" 1>&2
        exit 1
    fi
    cat << EOF
<Multi_key> <4> : "§"
<Multi_key> <е> : "€"
<Multi_key> <м> : "μ"

<Multi_key> <Cyrillic_yeru>     : "і"   # ы
<Multi_key> <Cyrillic_YERU>     : "І"   # Ы
<Multi_key> <Cyrillic_hardsign> : "ї"   # ъ
<Multi_key> <Cyrillic_HARDSIGN> : "Ї"   # Ъ
<Multi_key> <Cyrillic_e>        : "є"   # э
<Multi_key> <Cyrillic_E>        : "Є"   # Э
<Multi_key> <Cyrillic_io>       : "'"   # ё

Select what to type (press single character). Your choice:
EOF
    read -n1 INPUT_SYMBOL
    case "$INPUT_SYMBOL" in
        "4") OUTPUT_SYMBOL="§" ;;
        "е") OUTPUT_SYMBOL="€" ;;
        "м") OUTPUT_SYMBOL="μ" ;;
        "ы") OUTPUT_SYMBOL="і" ;;
        "Ы") OUTPUT_SYMBOL="І" ;;
        "ъ") OUTPUT_SYMBOL="ї" ;;
        "Ъ") OUTPUT_SYMBOL="Ї" ;;
        "э") OUTPUT_SYMBOL="є" ;;
        "Э") OUTPUT_SYMBOL="Є" ;;
        "ё") OUTPUT_SYMBOL="\"'\"" ;;
        *) OUTPUT_SYMBOL="" ;;
    esac
fi
if [ -n "$OUTPUT_SYMBOL" ]; then
    echo -n "$OUTPUT_SYMBOL" > $FILE
else
    echo -e "\n\nError: wrong input. Press enter to close this window"
    read
fi
