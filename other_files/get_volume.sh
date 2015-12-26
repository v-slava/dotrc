#!/bin/bash

set -e

amixer | head -n 6 | tail -n 1 | grep -o '[0-9]\+%'

