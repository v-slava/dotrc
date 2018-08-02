#!/bin/bash

set -e

amixer | grep 'Playback.*%\]' | head -n 1 | grep -o '[0-9]\+%'
