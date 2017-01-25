#!/bin/bash

usage()
{
	echo -e "$(basename $0) [-n] [-r] [NUM_COMMITS_BACK_FROM_HEAD]
-h                 => show this help
-n [ARG]           => git show --name-only HEAD[~ARG]
-r [ARG]           => git show --relative HEAD[~ARG]
-p [<path>...]     => git diff HEAD~1 HEAD -- [<path>...]
-P ARG [<path>...] => git diff HEAD~{ARG-1} HEAD~ARG -- [<path>...]

Note: ARG must be specified without '~'. For example:
$(basename $0) -n 1
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

SINGLE_ARG=0
while getopts "hnrpP:" arg ; do
	case "$arg" in
		h)
			usage 0
			;;
		n)
			SINGLE_ARG=1
			set_cmd "git show --name-only HEAD"
			;;
		r)
			SINGLE_ARG=1
			set_cmd "git show --relative HEAD"
			;;
		p)
			set_cmd "git diff HEAD~1 HEAD --"
			;;
		P)
			set_cmd "git diff HEAD~$((OPTARG - 1)) HEAD~$((OPTARG)) --"
			;;
	esac
done
shift $(($OPTIND - 1))

if [ $SINGLE_ARG -eq 1 ]; then
	if [ $# -gt 1 ]; then
		usage 2
	fi
	if [ -n "$1" ]; then
		CMD="${CMD}~${1}"
		shift
	fi
fi
$CMD "$@"
