#!/bin/bash

set -ex
cd ~/os_settings
git status
git stash list
git stash
git checkout master
git pull
git stash pop
git commit -a -m "Update configs"
git checkout second
git pull
git merge --no-edit master
git push origin master second
./apply_home_settings.sh
git show second
git show master
git status

