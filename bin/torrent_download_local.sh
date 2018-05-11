#!/bin/bash

set -e

scp -p $SSH_RASPBERRY_PI:downloads/* ~/downloads/
ssh -p $SSH_RASPBERRY_PI transmission-remote -t all -r
ssh -p $SSH_RASPBERRY_PI rm -rf downloads/*
