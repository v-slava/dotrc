#!/usr/bin/env bash

USAGE="Usage:\n\
$(basename $0) \"//depot/path/to/folder\" [changelist#]\n\
The folder will be downloaded to current directory."

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
	echo -e "$USAGE" 1>&2
	exit 1
fi

DEPOT_FOLDER="$1"
# Add last '/' to DEPOT_FOLDER if needed:
DEPOT_FOLDER_LEN=${#DEPOT_FOLDER}
DEPOT_FOLDER_LAST_SYMBOL=${DEPOT_FOLDER:$(($DEPOT_FOLDER_LEN-1)):1}
if [ "$DEPOT_FOLDER_LAST_SYMBOL" != "/" ]; then
	DEPOT_FOLDER="$(echo ${DEPOT_FOLDER}\/)"
fi
# Get folder name without path:
FOLDER_NAME="$(echo $DEPOT_FOLDER | rev | cut -d'/' -f 2 | rev)"

# Setup changelist#:
if [ $# -eq 2 ]; then
	CHANGELIST="$2"
	re='^[0-9]+$'
	if ! [[ $CHANGELIST =~ $re ]] ; then
	   echo -e "Error: Not a valid changelist: $CHANGELIST.\n$USAGE" 1>&2
	   exit 1
	fi
	CHANGELIST=$(echo @$CHANGELIST)
else
	CHANGELIST=#head
fi

CUR_RANDOM=$RANDOM
WORKSPACE_SETTINGS=$(echo "/tmp/temp_workspace_settings_${CUR_RANDOM}")
WORKSPACE_NAME=$(echo "DEV-PRJ_NAME${CUR_RANDOM}-v.volkov")

set -e

# Create workspace settings from template:
cat << 'EOF' > $WORKSPACE_SETTINGS
# A Perforce Client Specification.
#
#  Client:      The client name.
#  Update:      The date this specification was last modified.
#  Access:      The date this client was last used in any way.
#  Owner:       The Perforce user name of the user who owns the client
#               workspace. The default is the user who created the
#               client workspace.
#  Host:        If set, restricts access to the named host.
#  Description: A short description of the client (optional).
#  Root:        The base directory of the client workspace.
#  AltRoots:    Up to two alternate client workspace roots.
#  Options:     Client options:
#                      [no]allwrite [no]clobber [no]compress
#                      [un]locked [no]modtime [no]rmdir
#  SubmitOptions:
#                      submitunchanged/submitunchanged+reopen
#                      revertunchanged/revertunchanged+reopen
#                      leaveunchanged/leaveunchanged+reopen
#  LineEnd:     Text file line endings on client: local/unix/mac/win/share.
#  ServerID:    If set, restricts access to the named server.
#  View:        Lines to map depot files into the client workspace.
#  Stream:      The stream to which this client's view will be dedicated.
#               (Files in stream paths can be submitted only by dedicated
#               stream clients.) When this optional field is set, the
#               View field will be automatically replaced by a stream
#               view as the client spec is saved.
#  StreamAtChange:  A changelist number that sets a back-in-time view of a
#                   stream ( Stream field is required ).
#                   Changes cannot be submitted when this field is set.
#
# Use 'p4 help client' to see more about client views and options.

# Update:	2015/01/15 11:54:57

# Access:	2015/01/15 13:54:57

Owner:	v.volkov

Host:	laptop

Description:
	Created by v.volkov.

Options:	noallwrite noclobber nocompress unlocked nomodtime rmdir

SubmitOptions:	leaveunchanged

LineEnd:	share

EOF

# See output of "p4 client -o WORKSPACE_NAME"
echo "Root: $PWD/$FOLDER_NAME" >> $WORKSPACE_SETTINGS
echo "View: $DEPOT_FOLDER... //$WORKSPACE_NAME/..." >> $WORKSPACE_SETTINGS
echo "Client: $WORKSPACE_NAME" >> $WORKSPACE_SETTINGS

echo "$0: Create workspace: $WORKSPACE_NAME"
cat $WORKSPACE_SETTINGS | p4 client -i

echo "$0: Select workspace:"
export P4CLIENT=$WORKSPACE_NAME

set +e

echo "$0: Get revision: $CHANGELIST"
p4 sync "$DEPOT_FOLDER...$CHANGELIST"

echo "$0: Delete workspace:"
p4 client -d $WORKSPACE_NAME

if [ -d "$FOLDER_NAME" ]; then
	echo "$0: Add write permissions:"
	chmod -R u+w "$FOLDER_NAME"
fi

echo "$0: Done!"

