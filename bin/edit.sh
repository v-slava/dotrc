#!/bin/bash

# exec vim "$@"

if ! pgrep emacs 1>/dev/null ; then
	emacs --daemon
fi

is_a_number()
{
	local NUMBER_TO_CHECK="$1"
	echo "$NUMBER_TO_CHECK" | grep -q '^[0-9]\+$'
	return $?
}

CMD="emacsclient -c"
# "--wait" is my own argument to edit.sh.
# "--nofork" is actually an argument, accepted by vim
# (it is called with it from vifm when renaming a file/folder)
if [ "$1" = "--wait" ] || [ "$1" = "--nofork" ]; then
	shift
else
	CMD="$CMD -n"
fi

# process all command line arguments and as a result fill in $CMD:
for arg in "$@" ; do
	line="$(echo "$arg" | cut -d':' -f2)"
	if [ "$line" = "$arg" ] ; then
		CMD="$CMD $arg"
		continue
	fi
	if ! is_a_number "$line" ; then
		CMD="$CMD $arg"
		continue
	fi
	column="$(echo "$arg" | cut -d':' -f3)"
	if [ -z "$column" ]; then # if no column has been specified
		column=1 # then assume it equals to 1
	fi
	if ! is_a_number "$column" ; then
		CMD="$CMD $arg"
		continue
	fi
	file="$(echo "$arg" | cut -d':' -f1)"
	CMD="$CMD +${line}:${column} $file"
done

# execute final command:
$CMD

