#!/usr/bin/python3

import sys, os, email, pathlib, subprocess, email
from email.policy import default

notifications_to_show = 3
mail = os.path.join(pathlib.Path.home(), 'mail', 'isync')

# lock_dir = '/tmp/mbsync_lock_dir'
#
# try:
#     os.mkdir(lock_dir)
#     got_lock = True
# except (FileExistsError):
#     got_lock = False
#
# if got_lock:
#     subprocess.run(['mbsync', '-a'], check = True)
#     os.rmdir(lock_dir)

subprocess.run(['mbsync', '-a'], check = True)

paths = [os.path.join(mail, item) for item in os.listdir(mail)]
account_dirs = [d for d in paths if os.path.isdir(d) and not os.path.islink(d)]
for account_dir in account_dirs:
    new = os.path.join(account_dir, 'inbox', 'new')
    if not os.path.isdir(new):
        continue
    for eml_file in os.listdir(new):
        if notifications_to_show == 0:
            sys.exit(0)
        notifications_to_show = notifications_to_show - 1
        with open(os.path.join(new, eml_file), 'rb') as f:
            msg = email.message_from_binary_file(f, policy = default)
        From, Subject = msg['From'], msg['Subject']
        Subject = email.header.decode_header(Subject)[0][0]
        try:
            Subject = Subject.decode()
        except (UnicodeDecodeError, AttributeError):
            pass
        # In 5 minutes (300 000 ms) we will check for new emails once again.
        subprocess.run(['notify-send', '-u', 'low', '-t', '280000',
            f'Got new email from {From}:\n{Subject}'], check = True)
        # To mark new email as "seen":
        # mv inbox/new/${EMAIL_FILE} inbox/cur/${EMAIL_FILE}S
