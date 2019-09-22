#!/bin/bash

# With "set -e" the following "f" command can exit with a failure (in this case
# OUTPUT is empty). In this case this shell script will also exit with a failure
# and vifm will print "Press return ..." and wait for user to press "enter" key.
# To avoid this, we always exit with success here:
OUTPUT="$(f --no-clipboard)"
if [ $? -ne 0 ]; then
    exit 0
fi
vifm --server-name $VIFM_SERVER_NAME --remote -c "execute \"!echo \\\"$OUTPUT\\\" %IU\" | normal gf"
