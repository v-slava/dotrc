#!/bin/bash

set -e
svn status --no-ignore | grep '^[?I!A]      ' | cut -d' ' -f8- | xargs rm -rf
echo "+ svn status --no-ignore"
svn status --no-ignore
echo "+ svn status --no-ignore: exit code=$?"
