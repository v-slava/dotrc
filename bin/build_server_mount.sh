#!/usr/bin/env bash

sshfs -o allow_root,nonempty v.volkov@$IP_BUILD_SERVER:/ /media/build_server

