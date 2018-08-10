#!/bin/bash

LANG=C pacmd stat | grep 'Default source name: ' | cut -d':' -f2 | cut -d' ' -f 2
