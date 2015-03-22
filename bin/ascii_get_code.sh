#!/usr/bin/env bash

usage()
{
	echo "Usage: $(basename $0) {-d|-x|-X} SYMBOL" 1>&2
	exit 1
}

if [ $# -ne 2 ]; then
	usage
fi

if [ $1 = '-d' ]; then
	printf "%d\n" "'$2"
else
	if [ $1 = '-X' ]; then
		printf "0x%X\n" "'$2"
	else
		if [ $1 = '-x' ]; then
			printf "0x%x\n" "'$2"
		else
			usage
		fi
	fi
fi

