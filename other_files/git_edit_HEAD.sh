#!/bin/bash

set -e

GIT_ROOT_DIR="$(git rev-parse --show-toplevel)"

cd "$GIT_ROOT_DIR"
git show --name-only --pretty="" HEAD | xargs vim -p

