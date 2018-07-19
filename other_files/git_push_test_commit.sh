#!/bin/bash

source $DOTRC/other_files/git_fetch_push_test_commit_common.sh

git push --force $REPOSITORY HEAD:$BRANCH
