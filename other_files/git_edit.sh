#!/bin/bash

set -e

usage()
{
	echo "Usage: $(basename $0) {--commit|--diff}" 1>&2
	exit 1
}

if [ $# -ne 1 ]; then
	usage
fi

GIT_ROOT_DIR="$(git rev-parse --show-toplevel)"
cd "$GIT_ROOT_DIR"

case "$1" in
	"--commit")
		CMD="git show --name-only --pretty= HEAD"
		# CMD="git diff-tree --no-commit-id --name-only -r HEAD"
	;;
	"--diff")
		CMD="git diff --name-only --diff-filter=AM"
		# CMD="git diff --name-only --diff-filter=M"
		# CMD="git ls-files --others --full-name --exclude-standard"
	;;
	*)
		usage
	;;
esac

vim -p $($CMD)

