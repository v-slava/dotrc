#!/bin/bash

if [ $# -ne 3 ]; then
    echo "Usage: $0 SUB_DIR"
    exit 1
fi

SUB_DIR=$1

set -e

for branch in $(git ls-remote --heads origin | sed "s|.*/||"); do
    if ! git ls-tree -d origin/${branch}:${SUB_DIR} 2>/dev/null ; then
        continue # ${SUB_DIR} doesn't exist in this branch, so skip it
    fi
    git checkout $branch
done
git remote rm origin # just for safety reasons, we don't want to push to origin!
git filter-branch --tag-name-filter cat --prune-empty --subdirectory-filter $SUB_DIR -- --all
git reset --hard
git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
git reflog expire --expire=now --all
git gc --aggressive --prune=now
