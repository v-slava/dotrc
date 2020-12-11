#!/usr/bin/python3

import pexpect, sys, pathlib, subprocess

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description = 'ssh login.')
    parser.add_argument('ip', help = 'IP address', metavar = 'IP')
    return parser.parse_args()

def spawn(process):
    print(f'+ {process}')
    c = pexpect.spawn(process)
    c.logfile_read = sys.stdout.buffer
    return c

def ssh_keygen_retry(c, process, ip):
    c.expect('@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @')
    c.expect('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
    c.expect('IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!')
    c.expect('Someone could be eavesdropping on you right now \(man-in-the-middle attack\)!')
    c.expect('It is also possible that a host key has just been changed.')
    c.expect('The fingerprint for the ECDSA key sent by the remote host is')
    c.expect('SHA256:[^\n]+.')
    c.expect('Please contact your system administrator.')
    home = pathlib.Path.home()
    c.expect(f'Add correct host key in {home}/.ssh/known_hosts to get rid of this message.')
    c.expect(f'Offending ECDSA key in {home}/.ssh/known_hosts:[0-9]+')
    c.expect('  remove with:')
    cmd = f'  ssh-keygen -f "{home}/.ssh/known_hosts" -R "{ip}"'
    c.expect(cmd)
    c.expect(f'ECDSA host key for {ip} has changed and you have requested strict checking.')
    c.expect('Host key verification failed.')
    c.expect(pexpect.EOF)
    c.wait()
    print(f'+ {cmd}')
    subprocess.run(['ssh-keygen', '-f', f'{home}/.ssh/known_hosts', '-R',
        f'{ip}'], check = True)
    c = spawn(process)
    return c

def main():
    args = parse_cmd_line_args()
    process = f'ssh root@{args.ip}'
    password_prompt = f"root@{args.ip}'s password:"
    c = spawn(process)
    n = 0
    while n != 2:
        n = c.expect(['@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',
            "The authenticity of host [^\n]+ can't be established.",
            password_prompt])
        if n == 0: # got '@@@@@@...'
            c = ssh_keygen_retry(c, process, args.ip)
        elif n == 1: # got 'The authenticity of host ...'
            c.expect('ECDSA key fingerprint is SHA256:[^\n]+.')
            c.expect('Are you sure you want to continue connecting \(yes/no/\[fingerprint\]\)?')
            c.sendline('yes')
    c.sendline('our_password') # our password
    c.expect('/home/user # ')
    c.logfile_read = None
    c.interact()

if __name__ == '__main__':
    main()
