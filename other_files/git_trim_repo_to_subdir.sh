#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 SUB_DIR" 1>&2
    exit 1
fi

SUB_DIR=$1

set -e

# Delete all tags where ${SUB_DIR} doesn't exist:
for tag in $(git tag); do
    if ! git ls-tree -d ${tag}:${SUB_DIR} 1>/dev/null 2>&1 ; then
        # ${SUB_DIR} doesn't exist in this tag, so delete the tag:
        git tag -d $tag
    fi
done

# Delete all local branches:
git checkout --detach
for branch in $(git branch | grep -v '^\* '); do
    git branch --delete $branch
done

# Create local branches where ${SUB_DIR} exists:
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
