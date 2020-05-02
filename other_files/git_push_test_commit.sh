#!/bin/bash

# Use remote git repository to sync code (e.g. between windows and linux PCs).
# Here we push current HEAD to remote git repository overwriting remote history.

source $DOTRC/other_files/git_fetch_push_test_commit_common.sh

git push --force $REPOSITORY HEAD:$BRANCH
