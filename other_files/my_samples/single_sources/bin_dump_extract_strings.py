#!/usr/bin/env python3

# See also $DOTRC/other_files/my_samples/single_sources/uboot_mem_dump_extract_strings.sh

import sys
f = sys.argv[1]
i = open(f, "rb").read()
import string
pr = bytes(string.printable.encode('ascii'))
c = [b if b in pr else ord('\n') for b in i]
open(f, "wb").write(bytes(c))
