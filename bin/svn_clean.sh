#!/bin/bash

set -e
svn status | grep '^?      ' | cut -d' ' -f8- | xargs rm -rf
echo "+ svn status -q"
svn status -q
echo "+ svn status exit code: $?"
