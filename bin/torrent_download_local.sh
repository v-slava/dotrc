#!/bin/bash

set -e

scp -r -P $PI_PORT $PI_USR@$PI_HOST:downloads/* ~/downloads/
$PI_SSH transmission-remote -t all -r
$PI_SSH sudo rm -rf downloads/*
