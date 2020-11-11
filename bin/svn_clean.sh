#!/bin/bash

set -e
svn status | grep '^?      ' | cut -d' ' -f8- | xargs rm -rf
echo "+ svn status"
svn status
echo "+ svn status exit code: $?"
