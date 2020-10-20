#!/bin/bash

svn status | grep '^?      ' | cut -d' ' -f8- | xargs rm -rf
