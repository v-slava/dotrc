#!/bin/bash

set -e

case $1 in
    *.c | *.h | *.cpp | *.i | *.ii | *.hpp | *.cc)
        clang-format -i -style="{BasedOnStyle: Webkit, IndentWidth: 4}" "$1"
        ;;
    *.json)
        python3 -m json.tool "$1" /tmp/python_json_tool.json
        cp /tmp/python_json_tool.json "$1"
        ;;
    *.html)
        tidy -m "$1"
        ;;
esac
