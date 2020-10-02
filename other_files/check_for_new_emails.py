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

class EmailSettings():
    def __init__(self, user, host, port):
        self.user = user
        self.host = host
        self.port = port
    def __repr__(self):
        return f'{self.user}@{self.host}:{self.port}'

def import_file(full_name, path):
    from importlib import util
    spec = util.spec_from_file_location(full_name, path)
    mod = util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod

def get_email_settings(account):
    provider = account.split('@')[1]
    # Example email_settings.py:
    #
    # import imaplib
    #
    # def get_email_settings(account, provider):
    #     if provider != 'some_org.com':
    #         return (None, None, None)
    #     if account != 'Viacheslav.Volkov@some_org.com':
    #         return (None, None, None)
    #     return ('VVolkov', 'mail.some_org.com', imaplib.IMAP4_SSL_PORT)
    #
    dotrc_s_file = os.path.join(os.environ['DOTRC_S'], 'other_files',
            'email_settings.py')
    if os.path.isfile(dotrc_s_file):
        dotrc_s_module = import_file('my_email_settings', dotrc_s_file)
        user, host, port = dotrc_s_module.get_email_settings(account, provider)
        if user:
            assert(host)
            assert(port)
            return EmailSettings(user, host, port)
    user, host, port = {
            'gmail.com' : (account, f'imap.{provider}', imaplib.IMAP4_SSL_PORT),
            }.get(provider, (None, None, None))
    if not user:
        print(f'Error: unknown email provider: {provider}', file = sys.stderr)
        sys.exit(1)
    assert(host)
    assert(port)
    return EmailSettings(user, host, port)

pass_dir = os.path.join(os.environ['DOTRC_S'], 'other_files', 'settings_merge',
        'preprocess_include', 'passwords')
# In viacheslav.volkov.1@gmail.com.mbsyncrc :
# Pass "your_password"
for item in os.listdir(pass_dir):
    account_file = os.path.join(pass_dir, item)
    account = item[:-len('.mbsyncrc')]
    email_settings = get_email_settings(account)
    with open(account_file, 'r') as f:
        password = f.read()[len('Pass "'):-2]
    connection = imaplib.IMAP4_SSL(email_settings.host, email_settings.port)
    connection.login(email_settings.user, password)
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
