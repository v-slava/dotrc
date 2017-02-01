#!/bin/bash

usage()
{
	echo -e "\
$(basename $0) -h                 => show this help
$(basename $0) [-n] [-r] [ARG]    => git show [--name-only] [--relative] HEAD[~ARG]
$(basename $0) -p [<path>...]     => git diff HEAD~1 HEAD -- [<path>...]
$(basename $0) -P ARG [<path>...] => git diff HEAD~{ARG-1} HEAD~ARG -- [<path>...]
" 1>&2
	exit $1
}

set_cmd()
{
	if [ -n "$CMD" ]; then
		usage 1
	fi
	CMD="$1"
}

add_cmd()
{
	CMD="$CMD $1"
}

CMD="git show"
EXPECT_COMMIT_NUMBER=1
while getopts "hnrpP:" arg ; do
	case "$arg" in
		h)
			usage 0
			;;
		n)
			add_cmd "--name-only"
			;;
		r)
			add_cmd "--relative"
			;;
		p)
			set_cmd "git diff HEAD~1 HEAD --"
			EXPECT_COMMIT_NUMBER=0
			;;
		P)
			set_cmd "git diff HEAD~$((OPTARG - 1)) HEAD~$((OPTARG)) --"
			EXPECT_COMMIT_NUMBER=0
			;;
	esac
done
shift $(($OPTIND - 1))

if [ $EXPECT_COMMIT_NUMBER -eq 1 ]; then
	if [ $# -gt 1 ]; then
		usage 2
	fi
	CMD="$CMD HEAD"
	if [ -n "$1" ]; then
		CMD="${CMD}~${1}"
		shift
	fi
fi
$CMD "$@"
