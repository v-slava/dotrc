#!/bin/bash

tcpdump -i "$@" -w - -U | wireshark -k -i -

