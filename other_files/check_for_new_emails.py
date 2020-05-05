#!/usr/bin/python3

import sys, os, imaplib, email, subprocess

notifications_to_show = 3

def check(func):
    if ret != 'OK':
        print(f'{func}() failed for IMAP4_SSL connection', file = sys.stderr)
        connection.close()
        connection.logout()
        sys.exit(1)

def decode(arg):
    ret = ''
    for value, encoding in email.header.decode_header(arg):
        if type(value) is bytes:
            if encoding:
                ret += value.decode(encoding)
            else:
                ret += value.decode() # encoding is None => use default (UTF-8).
        else:
            assert type(value) is str
            ret += value
    return ret

pass_dir = os.path.join(os.environ['DOTRC_S'], 'other_files', 'settings_merge',
        'preprocess_include', 'passwords')
for item in os.listdir(pass_dir):
    account_file = os.path.join(pass_dir, item)
    account = item[:-len('.mbsyncrc')]
    provider = account.split('@')[1]
    host, port = {
            'gmail.com' : ('imap.gmail.com', imaplib.IMAP4_SSL_PORT),
            }.get(provider, (None, None))
    if not host:
        print(f'Error: unknown email provider: {provider}', file = sys.stderr)
        break
    with open(account_file, 'r') as f:
        password = f.read()[len('Pass "'):-2]
    connection = imaplib.IMAP4_SSL(host, port)
    connection.login(account, password)
    connection.select(mailbox = 'INBOX', readonly = True)
    ret, data = connection.search(None, 'UnSeen')
    check('search')
    for num in data[0].split():
        if notifications_to_show == 0:
            break
        notifications_to_show = notifications_to_show - 1
        ret, data = connection.fetch(num, '(RFC822)')
        check('fetch')
        msg = email.message_from_string(data[0][1].decode())
        From, Subject = decode(msg['From']), decode(msg['Subject'])
        # In 5 minutes (300 000 ms) we will check for new emails once again.
        subprocess.run(['notify-send', '-u', 'low', '-t', '280000',
            f'Got new email from {From}:\n{Subject}'], check = True)
        # To mark new email as "seen" for maildir (isync/mbsync):
        # mv inbox/new/${EMAIL_FILE} inbox/cur/${EMAIL_FILE}S
    connection.close()
    connection.logout()
