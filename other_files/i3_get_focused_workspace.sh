#!/bin/bash

i3-msg -t 'get_workspaces' | grep -o '"name":"[^"]\+","visible":true,"focused":true' | cut -d'"' -f4
