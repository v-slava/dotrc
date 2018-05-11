#!/bin/bash

set -e

scp -r -P $SSH_RASPBERRY_PI:downloads/* ~/downloads/
ssh -p $SSH_RASPBERRY_PI transmission-remote -t all -r
ssh -p $SSH_RASPBERRY_PI sudo rm -rf downloads/*
