#!/usr/bin/env python

from __future__ import print_function
from subprocess import *
import sys

# Input html file contains the following:
# <a href="SELECTED_URL"><img height="100" src="IMAGE_URL" width="150" alt="some text"></a>
# Need to extract IMAGE_URL knowing SELECTED_URL.
# The result should be saved in OUT_FILE.

TMP_DIR = "/tmp/anki"
IMAGE_WEB_PAGE = TMP_DIR + "/web_page.html"
TMP_DIR_IMAGE_FILE = TMP_DIR + "/image.jpg"

def error(*objs):
	print("Error: ", *objs, file=sys.stderr)
	sys.exit(1)

if len(sys.argv) < 2:
	error('SELECTED_URL is expected as command line argument')

ORIG_SELECTED_URL = sys.argv[1]
print(ORIG_SELECTED_URL)
start_index = ORIG_SELECTED_URL.index('http')
SELECTED_URL = ORIG_SELECTED_URL[start_index:]
print(SELECTED_URL)

with open(IMAGE_WEB_PAGE, 'r') as f:
	content = f.read()

selected_url_idx = content.index(SELECTED_URL)
content = content[selected_url_idx + len(SELECTED_URL):]
quote_idx = content.index('"')
content = content[quote_idx:]
img_idx = content.index('<img ')
content = content[img_idx:]
src_idx = content.index(' src="')
content = content[src_idx + len(' src="'):]
end_idx = content.index('"')
out_link = content[:end_idx]

check_call(["wget", "-O", TMP_DIR_IMAGE_FILE, out_link])
