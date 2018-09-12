#!/bin/bash

date1=`date +%s`
while true ; do
    days=$(( $(($(date +%s) - date1)) / 86400 ))
    echo -ne "$days day(s) and $(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r"
    sleep 0.1
done
