#!/usr/bin/env bash

set -e
FILE=$($PI_SSH "ls ~/uploads/current")
$PI_SSH "nohup woof ~/uploads/current/$FILE 1>~/uploads/log.txt 2>&1 &"
echo "Now serving on http://94.154.220.9:53536/$FILE"
