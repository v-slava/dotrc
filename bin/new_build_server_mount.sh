#!/usr/bin/env bash

sshfs -o allow_root,nonempty bld@$IP_NEW_BUILD_SERVER:/ /media/new_build_server

