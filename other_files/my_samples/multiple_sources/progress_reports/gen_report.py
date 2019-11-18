#!/usr/bin/python3

import re
import os
import sys
import shutil
import locale
import datetime
import calendar
import subprocess
import webbrowser
from collections import namedtuple
from tempfile import NamedTemporaryFile
from email.message import EmailMessage
from email.generator import Generator

def parse_cmd_line_args():
    import argparse
    time_period_file = get_time_period_file_relative()
    parser = argparse.ArgumentParser(description =
            'Generate final progress report from individual reports.')
    parser.add_argument('--report-due', choices = ['this_week', 'next_week'],
            help = f"update report period dates in {time_period_file}. Set \
report period to be 2 weeks ending on Friday (either this week's Friday or \
next week's Friday)")
    parser.add_argument('--check-updates', action = 'store_true',
            help = 'check whether every contributor updated his/her report in \
this repository during report period')
    parser.add_argument('--check-html', action = 'store_true',
            help = 'check whether html to be generated is valid ("tidy" tool \
is required)')
    parser.add_argument('--view-html', action = 'store_true',
            help = 'view HTML in web browser')
    parser.add_argument('--html-to-clipboard', action = 'store_true',
            help = 'copy HTML body to clipboard.'
                 + ' On linux either "xclip" or "xsel" is required')
    parser.add_argument('--html', metavar = 'FILE',
            help = 'file to write report in HTML format to')
    parser.add_argument('--eml', metavar = 'FILE',
            help = 'file to write report in EML format to')
    parser.add_argument('--outlook', action = 'store_true',
            help = 'edit report email in outlook (on windows only)')
    args = parser.parse_args()
    if not any(vars(args).values()):
        parser.print_help(sys.stderr)
        parser.exit(status = 0, message =
                '\nNo arguments provided => nothing to do. Exiting...\n')
    if args.outlook and sys.platform != 'win32':
        parser.error('--outlook is not supported on this platform')
    return args

def get_git_root_dir():
    return os.path.dirname(os.path.realpath(__file__))

def get_data_dir():
    return os.path.join(get_git_root_dir(), 'data')

def get_time_period_file():
    return os.path.join(get_data_dir(), 'email', 'report_time_period.txt')

def get_time_period_file_relative():
    return os.path.relpath(get_time_period_file(), get_git_root_dir())

def get_projects_dir():
    return os.path.join(get_data_dir(), 'projects')

def get_from_git_config(value):
    cmd = ['git', '-C', get_git_root_dir(), 'config', value]
    ret = subprocess.run(cmd, check = True, capture_output = True,
            encoding = 'utf8')
    return ret.stdout.rstrip()

def update_time_period(report_due):
    today = datetime.date.today()
    today_weekday = today.weekday()
    if report_due == 'this_week':
        if today_weekday > calendar.FRIDAY:
            raise Exception("this week's Friday is already in the past")
        days_left = calendar.FRIDAY - today_weekday
    else:
        assert report_due == 'next_week'
        days_left = 7 + calendar.FRIDAY - today_weekday
    end = today + datetime.timedelta(days = days_left)
    start = end - datetime.timedelta(days = 13) # 2 weeks - 1 day
    start_date, end_date = start.isoformat(), end.isoformat()
    with open(get_time_period_file(), 'w') as f:
        f.write(f'{start_date} - {end_date}\n')
    get_time_period.cache = start, end

def get_time_period():
    if hasattr(get_time_period, 'cache'):
        return get_time_period.cache
    with open(get_time_period_file()) as f:
        s = f.readlines()
    time_period_file = get_time_period_file_relative()
    exception_str = f'Invalid format of {time_period_file}. Expected format: \
yyyy-mm-dd - yyyy-mm-dd'
    if len(s) != 1:
        raise Exception(exception_str)
    strdates = s[0].rstrip().split(' - ')
    if len(strdates) != 2:
        raise Exception(exception_str)
    start, end = [datetime.date.fromisoformat(d) for d in strdates]
    if start >= end:
        raise Exception(f'start date >= end date in {time_period_file}')
    get_time_period.cache = start, end
    return get_time_period.cache

def check_updates():
    start, end = get_time_period()
    git_root_dir = get_git_root_dir()
    time_period_file = get_time_period_file_relative()
    git_cmd = ['git', '-C', git_root_dir, 'log', '-1', '--pretty=format:%cd',
            '--date=short']
    files_to_check = []
    f_name = 'check_updates.txt'
    def git_file(f):
        return os.path.relpath(os.path.join(root, f), git_root_dir).rstrip()
    for root, subdirs, files in os.walk(get_projects_dir()):
        if f_name in files:
            with open(os.path.join(root, f_name), encoding = 'utf8') as f:
                files_to_check += [git_file(f) for f in f.readlines()]
    for f in files_to_check:
        ret = subprocess.run(git_cmd + [f], check = True, capture_output = True,
                encoding = 'ascii')
        modified = datetime.date.fromisoformat(ret.stdout)
        if modified > end:
            raise Exception(f'file:\n{f}\nis modified after end date in'
                    + f' {time_period_file}')
        if modified < start:
            raise Exception(f'file:\n{f}\nis not updated according to'
                    + f' {time_period_file}')

def get_html_string():
    def process_file(directory, file_name):
        nonlocal html_lines
        file_path = os.path.join(directory, file_name)
        with open(file_path, encoding = 'utf8') as f:
            for line in f:
                match = include_re.match(line)
                if match:
                    included_file = match.group(1)
                    d_name = os.path.dirname(included_file)
                    d_name = os.path.join(directory, d_name)
                    f_name = os.path.basename(included_file)
                    process_file(d_name, f_name)
                else:
                    line = line.replace('#GIT_USER_NAME', git_user_name)
                    html_lines.append(line)
    html_lines = []
    include_re = re.compile(r'.*#INCLUDE_FILE: (\S+)')
    git_user_name = get_from_git_config('user.name')
    process_file(get_data_dir(), 'index.html')
    return ''.join(html_lines)

def check_html(html_file):
    if not shutil.which('tidy'):
        raise Exception('"tidy" is not installed')
    cmd = ['tidy', '-e', '-q', html_file]
    try:
        subprocess.run(cmd, check = True, stderr = subprocess.PIPE,
                encoding = 'utf8')
    except subprocess.CalledProcessError as e:
        print('"tidy" detected an issue in HTML:', file = sys.stderr)
        sys.stderr.write(e.stderr)
        raise e

def view_html(html_file):
    webbrowser.open(html_file)

def copy_to_clipboard(html_string):
    if sys.platform == 'win32':
        cmd = ['clip']
        try:
            html_string.encode('utf8').decode('ascii')
        except UnicodeDecodeError:
            raise Exception('''HTML contains non-ascii characters. Since \
windows tool "clip" is known to have issues with utf8, we can't copy this HTML \
to clipboard''')
    else:
        if shutil.which('xclip'):
            cmd = ['xclip', '-selection', 'clipboard']
        elif shutil.which('xsel'):
            cmd = ['xsel', '--clipboard', '-i']
        else:
            raise Exception('neither "xclip" nor "xsel" is installed')
    subprocess.run(cmd, check = True, input = html_string, encoding = 'utf8')

def get_reporters_emails():
    members = []
    for root, subdirs, files in os.walk(get_projects_dir()):
        if os.path.basename(root) == 'members':
            members += subdirs
    return [m + '@your_company.com' for m in members]

def get_email_subject():
    def english(date):
        locale.setlocale(locale.LC_TIME, 'C')
        use_leading_zero = '#' if sys.platform == 'win32' else '-'
        return date.strftime(f'%{use_leading_zero}d %b')
    def chinese(date):
        locale.setlocale(locale.LC_TIME, 'zh_CN.UTF-8')
        return date.strftime('%b%-d日')
    def get_period(start, end, lang_func):
        return lang_func(start) + ' - ' + lang_func(end)
    start, end = get_time_period()
    subject = ''
    en_period = get_period(start, end, english)
    # subject += f"your team's Biweekly Report ({en_period})"
    subject += f'Biweekly report ({en_period})'

    return subject

def email_msg(to, cc, bcc, subject, body, html = False):
    msg = EmailMessage()
    msg['X-Unsent'] = '1'
    if to:
        msg['To'] = to
    if cc:
        msg['Cc'] = cc
    if bcc:
        msg['Bcc'] = bcc
    if subject:
        msg['Subject'] = subject
    msg.set_content(body, subtype = 'html' if html else 'plain')
    return msg

def get_email_msg(html_body):
    from data.email import recipients
    def remove_duplicates(l):
        return list(dict.fromkeys(l))
    git_user_email = get_from_git_config('user.email')
    def remove_self(l):
        if git_user_email in l:
            l.remove(git_user_email)
    def filter_email_list(l):
        result = remove_duplicates(l)
        remove_self(result)
        return result
    to = filter_email_list(recipients.email_to)
    cc = filter_email_list(recipients.email_cc + get_reporters_emails())
    bcc = filter_email_list(recipients.email_bcc)
    subject = get_email_subject()
    msg = email_msg(to, cc, bcc, subject, html_body, html = True)
    return msg

def write_email_msg_to_file(f, email_msg):
    g = Generator(f)
    g.flatten(email_msg)

def open_outlook(eml_file):
    outlook_exe = 'C:\Program Files\Microsoft Office\Office15\outlook.exe'
    subprocess.check_call([outlook_exe, '/eml', os.path.realpath(eml_file)])

def main():
    args = parse_cmd_line_args()

    if args.report_due:
        update_time_period(args.report_due)

    if args.check_updates:
        check_updates()

    html_string = get_html_string()
    if args.html:
        with open(args.html, 'w', encoding = 'utf8') as f:
            f.write(html_string)

    def process_html_file(html_file):
        if args.check_html:
            check_html(html_file)
        if args.view_html:
            view_html(html_file)

    if args.check_html or args.view_html or args.html_to_clipboard:
        if args.html:
            process_html_file(args.html)
        else:
            f = NamedTemporaryFile(mode = 'w', suffix = '.html',
                    encoding = 'utf8', delete = False)
            f.write(html_string)
            f.close()
            process_html_file(f.name)
            # In case process_html_file() raises an exception we don't want to
            # delete this temporary file.
            # On windows for google chrome webbrowser.open() returns before the
            # page was fully loaded (bug?), so if we delete the file here, it
            # will fail to load the page. Therefore do not delete the temporary
            # file in this case.
            if not ((sys.platform == 'win32') and args.view_html):
                os.unlink(f.name)

    if args.html_to_clipboard:
        copy_to_clipboard(html_string)

    email_msg = get_email_msg(html_string)
    if args.eml:
        with open(args.eml, 'w', encoding = 'utf8') as f:
            write_email_msg_to_file(f, email_msg)
        if args.outlook:
            open_outlook(args.eml)
    else:
        if args.outlook:
            with NamedTemporaryFile(mode = 'w', suffix = '.eml',
                    encoding = 'utf8', delete = False) as f:
                write_email_msg_to_file(f, email_msg)
                f.close()
                open_outlook(f.name)

if __name__ == '__main__':
    main()
