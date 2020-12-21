#!/usr/bin/python3

import sys, os, imaplib, email, subprocess, re

notifications_to_show = 3
dotrc_s_file = os.path.join(os.environ['DOTRC_S'], 'other_files',
        'email_settings.py')

def check_ret(func, ret, data, connection):
    if ret != 'OK':
        print(f'"{func}" failed for IMAP4_SSL connection: ({ret}, {data})',
                file = sys.stderr)
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
    def __init__(self, user, host, port, spam_folder):
        self.user = user
        self.host = host
        self.port = port
        self.spam_folder = spam_folder
    def __repr__(self):
        return f'{self.user}@{self.host}:{self.port}, spam_folder = {self.spam_folder}'

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
    # import imaplib, email, re
    #
    # def decode(arg):
    #     ret = ''
    #     for value, encoding in email.header.decode_header(arg):
    #         if type(value) is bytes:
    #             if encoding:
    #                 ret += value.decode(encoding)
    #             else:
    #                 ret += value.decode()
    #         else:
    #             assert type(value) is str
    #             ret += value
    #     return ret
    #
    # def get_email_settings(account, provider):
    #     if provider != 'some_org.com':
    #         return (None, None, None, None)
    #     if account != 'Viacheslav.Volkov@some_org.com':
    #         return (None, None, None, None)
    #     return ('VVolkov', 'mail.some_org.com', imaplib.IMAP4_SSL_PORT,
    #             '"Junk Email"') # 'Spam' for gmail
    #
    # def is_spam(msg, account):
    #     if account != 'Viacheslav.Volkov@some_org.com':
    #         return False
    #     From = decode(msg['From'])
    #     if From != 'Viacheslav Volkov <viacheslav.volkov.1@gmail.com>':
    #         return False
    #     Subject = decode(msg['Subject'])
    #     spam = 'Spam msg # \d+'
    #     if not re.match(spam, Subject):
    #         return False
    #     return True
    #
    if os.path.isfile(dotrc_s_file):
        dotrc_s_module = import_file('my_email_settings', dotrc_s_file)
        if hasattr(dotrc_s_module, 'get_email_settings'):
            ret = dotrc_s_module.get_email_settings(account, provider)
            user, host, port, spam_folder = ret
            if user:
                assert(host)
                assert(port)
                assert(spam_folder)
                return EmailSettings(user, host, port, spam_folder)
    user, host, port, spam_folder = {
    'gmail.com' : (account, f'imap.{provider}', imaplib.IMAP4_SSL_PORT, 'Spam'),
    }.get(provider, (None, None, None, None))
    if not user:
        print(f'Error: unknown email provider: {provider}', file = sys.stderr)
        sys.exit(1)
    assert(host)
    assert(port)
    assert(spam_folder)
    return EmailSettings(user, host, port, spam_folder)

def is_spam(msg, account):
    if account == 'viacheslav.volkov.1@gmail.com':
        From = decode(msg['From'])
        Subject = decode(msg['Subject'])
        if From == 'ROZETKA <bestdeal@rozetka.com.ua>':
            if Subject.startswith('Залиште відгук про куплений товар'):
                return True
    if not os.path.isfile(dotrc_s_file):
        return False
    dotrc_s_module = import_file('my_email_settings', dotrc_s_file)
    if not hasattr(dotrc_s_module, 'is_spam'):
        return False
    return dotrc_s_module.is_spam(msg, account)

def get_msg_uid(connection, msg_num):
    def check(func):
        check_ret(func, ret, data, connection)
    ret, data = connection.fetch(msg_num, '(UID)') # data == [b'34 (UID 11849)']
    check('fetch')
    data_str = data[0].decode('utf8')
    msg_uid = re.match('\d+ \(UID (?P<uid>\d+)\)', data_str).group('uid')
    return msg_uid

def move_msg_to_folder(connection, msg_num, spam_folder):
    def check(func):
        check_ret(func, ret, data, connection)
    msg_uid = get_msg_uid(connection, msg_num)
    ret, data = connection.uid('COPY', msg_uid, spam_folder)
    check(f'COPY -> {spam_folder}')
    ret, data = connection.uid('STORE', msg_uid, '+FLAGS', '(\Deleted)')
    check('STORE +Deleted')

# def mark_msg_as_unseen(connection, msg_num):
#     def check(func):
#         check_ret(func, ret, data, connection)
#     msg_uid = get_msg_uid(connection, msg_num)
#     ret, data = connection.uid('STORE', msg_uid, '-FLAGS', '(\Seen)')
#     check('STORE -Seen')

def main():
    def check(func):
        check_ret(func, ret, data, connection)
    pass_dir = os.path.join(os.environ['DOTRC_S'], 'other_files',
            'settings_merge', 'preprocess_include', 'passwords')
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
        # print(f'\n{account}:') # print folders for all accounts
        # for i in connection.list()[1]:
        #     folder = i.decode('utf8').split(' "/" ')[1]
        #     print(f"{folder}")
        # continue
        connection.select(mailbox = 'INBOX', readonly = False)
        ret, data = connection.search(None, 'UnSeen')
        check('search')
        for msg_num in data[0].split():
            ret, data = connection.fetch(msg_num, '(BODY.PEEK[HEADER])') # '(RFC822)'
            check('fetch')
            msg = email.message_from_string(data[0][1].decode())
            if is_spam(msg, account):
                move_msg_to_folder(connection, msg_num, email_settings.spam_folder)
                continue
            global notifications_to_show
            if notifications_to_show == 0:
                continue
            notifications_to_show = notifications_to_show - 1
            From, Subject = decode(msg['From']), decode(msg['Subject'])
            # In 5 minutes (300 000 ms) we will check for new emails once again.
            subprocess.run(['notify-send', '-u', 'low', '-t', '280000',
                f'Got new email from {From}:\n{Subject}'], check = True)
            # To mark new email as "seen" for maildir (isync/mbsync):
            # mv inbox/new/${EMAIL_FILE} inbox/cur/${EMAIL_FILE}S
        connection.expunge()
        connection.close()
        connection.logout()

if __name__ == '__main__':
    main()
