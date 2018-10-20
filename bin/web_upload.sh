#!/usr/bin/env bash

set -e
FILE=$($PI_SSH "ls ~/uploads")
$PI_SSH "nohup woof ~/uploads/$FILE 1>~/log_upload 2>&1 &"
echo "Now serving on http://94.154.220.9:53536/$FILE"
