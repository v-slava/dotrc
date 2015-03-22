#!/usr/bin/env bash

sshfs -o allow_root,nonempty v.volkov@seclab.surc.kiev.ua:/home/exchange /media/exchange
# sudo mount -t cifs -o user=v.volkov,domain=SURC //seclab.surc.kiev.ua/Exchange /media/exchange

