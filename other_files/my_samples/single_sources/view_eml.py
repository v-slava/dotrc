#!/usr/bin/env python3

"""View eml file, extract into a directory of files."""

import os
import email
from email.policy import default
import mimetypes
import argparse

def main():
    parser = argparse.ArgumentParser(description="""\
View eml file, extract into a directory of files.
""")
    parser.add_argument('file')
    args = parser.parse_args()
    directory = 'extracted_' + args.file

    with open(args.file, 'rb') as fp:
        msg = email.message_from_binary_file(fp, policy = default)

    Date, From, To = msg['Date'], msg['From'], msg['To']
    Subject = msg['Subject']
    # Subject = '=?UTF-8?B?8J+TtyBEb2xpYSBBbmRyaWkg?= =?UTF-8?B?YWRkZWQgYSBuZXcgcGhv?= =?UTF-8?B?dG8=?='
    Subject = email.header.decode_header(Subject)[0][0]
    print(f'Date: {Date}\nFrom: {From}\nTo: {To}')
    print(f'Subject: {Subject}')
    # f0 9f 93 b7 = U+1F4F7 = CAMERA (utf8 emoji)
    # http://www.fileformat.info/info/unicode/char/2248/fontsupport.htm

    try:
        os.mkdir(directory)
    except FileExistsError:
        pass

    counter = 1
    for part in msg.walk():
        # multipart/* are just containers
        if part.get_content_maintype() == 'multipart':
            continue
        # Applications should really sanitize the given filename so that an
        # email message can't be used to overwrite important files
        filename = part.get_filename()
        if not filename:
            ext = mimetypes.guess_extension(part.get_content_type())
            if not ext:
                # Use a generic bag-of-bits extension
                ext = '.bin'
            filename = f'{counter}_part{ext}' # {counter:03d}
        counter += 1
        with open(os.path.join(directory, filename), 'wb') as fp:
            fp.write(part.get_payload(decode = True))

if __name__ == '__main__':
    main()
