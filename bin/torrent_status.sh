#!/bin/sh

# ssh -p $SSH_RASPBERRY_PI transmission-remote -l
ssh -p $SSH_RASPBERRY_PI TERM=rxvt-unicode-256color watch -d -n 1 transmission-remote -l
