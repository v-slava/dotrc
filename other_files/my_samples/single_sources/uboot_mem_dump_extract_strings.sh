#!/bin/bash

# In uboot console use command like:
# md.b address_of(__log_buf) num_bytes
# For example:
# md.b 0xc0a3802c 3000

# See also $DOTRC/usefull_info.txt on how to get address_of(__log_buf)
# See also $DOTRC/other_files/my_samples/single_sources/bin_dump_extract_strings.py

address=0
cut -d: -f2- | cut -d' ' -f -20 | while read line ; do
    printf "%04x: %s\n" $address "$line"
    address=$((address + 0x10))
done | xxd -r | strings
