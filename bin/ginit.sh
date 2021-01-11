#!/bin/bash

EMAIL="$1"

set -e

usage()
{
    echo -e "Usage: $(basename $0) {p|w}

p - use personal email
w - use work email
" 1>&2
    exit 1
}

if [ -z "$EMAIL" ]; then
    usage
fi

DIR=$DOTRC_S/other_files/settings_merge/preprocess_include/passwords
ACCOUNTS="$(ls $DIR/ | rev | cut -c 10- | rev)"
PERSONAL_PROVIDER=gmail

case "$EMAIL" in
    p) EMAIL=$(echo "$ACCOUNTS" | grep $PERSONAL_PROVIDER) ;;
    w) EMAIL=$(echo "$ACCOUNTS" | grep -v $PERSONAL_PROVIDER) ;;
    *) usage ;;
esac

GIT_WORK_EMAIL=$DOTRC_S/other_files/git_work_email.sh
if [ -f $GIT_WORK_EMAIL ]; then
    . $GIT_WORK_EMAIL
fi

set -x
git init .
git config user.email "$EMAIL"
