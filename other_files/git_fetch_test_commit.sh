#!/bin/bash

# Use remote git repository to sync code (e.g. between windows and linux PCs).
# Here we fetch remote repository and checkout remote branch in current
# workspace overwriting local history.


source $DOTRC/other_files/git_fetch_push_test_commit_common.sh

git fetch $REPOSITORY
git checkout -B $BRANCH
git reset $REPOSITORY/$BRANCH --hard
