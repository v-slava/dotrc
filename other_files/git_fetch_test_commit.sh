#!/bin/bash

source ~/os_settings/other_files/git_fetch_push_test_commit_common.sh

git fetch $REPOSITORY
git checkout -B $BRANCH
git reset $REPOSITORY/$BRANCH --hard
