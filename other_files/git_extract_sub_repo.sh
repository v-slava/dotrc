#!/bin/bash

if [ $# -ne 3 ]; then
    echo "Usage: $0 <git-source> <repo-name> <subdir>"
    exit 1
fi

GIT_SOURCE=$1
REPO_NAME=$2
SUB_DIR=$3

set -e

git clone $GIT_SOURCE $REPO_NAME

cd $REPO_NAME
for branch in $(git ls-remote --heads origin | sed "s|.*/||"); do
    git checkout $branch
done
git remote rm origin
git filter-branch --tag-name-filter cat --prune-empty --subdirectory-filter $SUB_DIR -- --all
git reset --hard
git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
git reflog expire --expire=now --all
git gc --aggressive --prune=now
