#!/bin/bash

usage()
{
    echo -e "Usage: cd SVN_REPO_ROOT_DIR && $(basename $0) [-v '+ '] PATCH_FILE" 1>&2
    exit 1
}

set -e
MY_ECHO_PREFIX=
if [ "$1" = "-v" ]; then
    shift
    MY_ECHO_PREFIX="$1"
    if [ -z "$MY_ECHO_PREFIX" ]; then
        usage
    fi
    shift
fi
PATCH_FILE="$1"

if [ -z "$PATCH_FILE" ]; then
    usage
fi

if [ ! -d ".svn" ]; then
    usage
fi

OUTPUT="$(svn status --no-ignore)"
if [ -n "$OUTPUT" ]; then
    echo "svn repository is not clean => aborting (no changes have been made)\
..." 1>&2
    exit 1
fi

my_echo()
{
    if [ -n "$MY_ECHO_PREFIX" ]; then
        echo "${MY_ECHO_PREFIX}$1"
    fi
}

add()
{
    FILE="$1"
    my_echo "svn add \"$FILE\""
    svn add "$FILE"
}

remove()
{
    FILE="$1"
    my_echo "svn remove \"$FILE\""
    svn remove "$FILE"
}

FIRST_LINE="$(tail -n +4 "$PATCH_FILE" | head -n1 | cut -d' ' -f4-)"
SECOND_LINE="$(tail -n +5 "$PATCH_FILE" | head -n1)"
if [ -n "$SECOND_LINE" ]; then
    FIRST_LINE="${FIRST_LINE}${SECOND_LINE}"
    THIRD_LINE="$(tail -n +6 "$PATCH_FILE" | head -n1)"
    # Note: always keep 2nd line in git commit message empty for this to work.
    if [ -n "$THIRD_LINE" ]; then
        echo "Error: unexpected patch file format for: \"$PATCH_FILE\"" 1>&2
        exit 1
    fi
fi
EMPTY_LINE=$(grep -n '^$' "$PATCH_FILE" | head -n1 | cut -d: -f1)
LAST_LINE=$(tail -n +$(($EMPTY_LINE + 1)) "$PATCH_FILE" | grep -n '^---$' | head -n1 | cut -d: -f1)
COMMIT_MESSAGE="$(
    echo -e "$FIRST_LINE\n"
    tail -n +$(($EMPTY_LINE + 1)) "$PATCH_FILE" | head -n $(($LAST_LINE - 1))
)"

my_echo "svn patch \"$PATCH_FILE\""
svn patch "$PATCH_FILE"

svn status --no-ignore | while read LINE ; do
    FILE_PATH="$(echo "$LINE" | cut -c 9-)"
    FILE_STATUS="$(echo "$LINE" | cut -c 1)"
    case "$FILE_STATUS" in
        "?") add "$FILE_PATH" ;;
        "M") ;; # do nothing
        "A") ;; # do nothing
        "!") remove "$FILE_PATH" ;;
        *)   echo "Unexpected svn file status: \"$FILE_STATUS\" for \
\"$FILE_PATH\"" 1>&2 ; exit 1 ;;
    esac
done

my_echo "svn commit -m \"<extracted from patch file commit message>\""
svn commit -m "$COMMIT_MESSAGE"
my_echo "svn up"
svn up
