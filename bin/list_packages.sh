#!/bin/bash

ls -lt --time-style=long-iso /var/lib/dpkg/info/*.list | awk '{print $6,$7,$8,$9}' | rev | cut -d'.' -f 2- | rev | awk -F'/' '{print $1,$6}'
