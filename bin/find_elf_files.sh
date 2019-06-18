#!/bin/bash

find -type f -executable | while read file ; do
    MIME_TYPE=$(file -b --mime-type $file)
    case "$MIME_TYPE" in
        "application/x-executable" | "application/x-sharedlib")
            echo $file
            ;;
        *)
            ;;
    esac
done
