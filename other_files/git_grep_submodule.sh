#!/bin/bash

ROOT_DIR="$1"
shift
PREFIX="$(realpath --relative-to="$ROOT_DIR" "$PWD")"
git grep "$@" | sed -e "s/^/$PREFIX\//"
