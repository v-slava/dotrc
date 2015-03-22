#!/usr/bin/env bash

# Usage: ./get_p4_files_list.sh > files_list
p4 files '//...' | cut -d'#' -f1

