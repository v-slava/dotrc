#!/bin/bash

command()
{
    ls -lt --time-style=long-iso /var/lib/dpkg/info/*.list | awk '{print $6,$7,$8,$9}' | rev | cut -d'.' -f 2- | rev | awk -F'/' '{print $1,$6}'
}

if [ $# -eq 0 ]; then
    command
    exit
fi

LAST_DATE=$(command | head -n1 | cut -d' ' -f1)

packets()
{
    command | grep "^$LAST_DATE " | cut -d' ' -f4
}

if [ "$1" = "-n" ]; then
    packets
fi

if [ "$1" = "-nd" ]; then
    packets | xargs apt-get purge -y
fi
