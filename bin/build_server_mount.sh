#!/usr/bin/env bash

sshfs -o allow_root,nonempty bld@$IP_BUILD_SERVER:/ /media/build_server

