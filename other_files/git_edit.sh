#!/bin/bash

set -e

usage()
{
	echo "Usage: $(basename $0) {c|d} [Gdiff_arg]
c - files from current commit
d - files from diff
Possible Gdiff_arg values:
h      - HEAD
NUMBER - ~NUMBER." 1>&2
	exit 1
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
	usage
fi

GIT_ROOT_DIR="$(git rev-parse --show-toplevel)"
cd "$GIT_ROOT_DIR"

case "$1" in
	"c")
		CMD="git show --name-only --pretty= HEAD"
		# CMD="git diff-tree --no-commit-id --name-only -r HEAD"
	;;
	"d")
		CMD="git diff --name-only --diff-filter=AM"
		# CMD="git diff --name-only --diff-filter=M"
		# CMD="git ls-files --others --full-name --exclude-standard"
	;;
	*)
		usage
	;;
esac

# Filter out binary files:
for file in $($CMD) ; do
	if file -ib "$file" | grep -q "^text/" ; then
		FILES_LIST="$FILES_LIST $file"
	fi
done

case "$2" in
	"h")
		GDIFF_ARG="HEAD"
	;;
	"")
		GDIFF_ARG=""
	;;
	*)
		GDIFF_ARG="~$2"
	;;
esac

vim -p $FILES_LIST -c "let g:Gdiff_arg='$GDIFF_ARG'|exec ':Gdiff ' . g:Gdiff_arg"
