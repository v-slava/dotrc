#!/bin/bash

set -e

# ~/os_settings/other_files/lock_screen.sh
x-terminal-emulator -title poweroff -e bash -c 'source ~/.bashrc && exec ~/os_settings/other_files/shutdown.sh'

