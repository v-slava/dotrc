#!/usr/bin/env python3

import subprocess

def compose_mail_to(to, cc, bcc, subject, body):
    def email_recipients(emails):
        return '; '.join(emails)
    def escape(text):
        return text.replace(' ', '%20').replace('\n', '%0A')
    args = email_recipients(to)
    if cc:
        args += '&cc=' + email_recipients(cc)
    if bcc:
        args += '&bcc=' + email_recipients(bcc)
    if subject:
        args += '&subject=' + escape(subject)
    if body:
        args += '&body=' + escape(body)
    outlook_exe = 'C:\Program Files\Microsoft Office\Office15\outlook.exe'
    cmd = [outlook_exe, '/c', 'ipm.note', '/m', args]
    # /f msgfilename      Opens the specified message file (.msg) or
    #                     Microsoft Office saved search (.oss).
    # /eml file.eml       Opens the specified .eml file
    # To open .eml file in compose mode in outlook, add the following header
    # to the file:
    # X-Unsent: 1
    # Python module: import email
    subprocess.check_call(cmd)

to = ('recipient_1@gmail.com', 'recipient_2@gmail.com')
cc = () ; bcc = [] # cc = to ; bcc = to
compose_mail_to(to, cc, bcc, 'some subject', 'line 1\nline 2')
