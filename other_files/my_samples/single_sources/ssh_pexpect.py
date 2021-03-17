#!/usr/bin/python3

import pexpect, sys, pathlib, subprocess

def parse_cmd_line_args():
    import argparse
    parser = argparse.ArgumentParser(description = 'ssh login.')
    parser.add_argument('-c', '--command', help = 'shell command to execute',
            metavar = 'CMD', nargs = '?')
    parser.add_argument('-i', '--interact', action = 'store_true',
            help = 'give control to user after executing shell command')
    parser.add_argument('target_ip', help = 'target IP address',
            metavar = 'TARGET_IP')
    args = parser.parse_args()
    if not args.command:
        args.interact = True
    return args

def spawn(process):
    print(f'+ {" ".join(process)}')
    c = pexpect.spawn(process[0], process[1:])
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
    process = ['ssh', f'root@{args.target_ip}']
    # if args.command:
    #     process += [f'{args.command}']
    # process = f'ssh-h root@{args.target_ip}'
    password_prompt = f"root@{args.target_ip}'s password:"
    c = spawn(process)
    n = 0
    while n != 3:
        n = c.expect(['@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',
            "The authenticity of host [^\n]+ can't be established.",
            f'ssh: connect to host {args.target_ip} port 22: No route to host',
            # pexpect.TIMEOUT,
            password_prompt]) # , timeout = 300
        if n == 0: # got '@@@@@@...'
            c = ssh_keygen_retry(c, process, args.target_ip)
        elif n == 1: # got 'The authenticity of host ...'
            c.expect('ECDSA key fingerprint is SHA256:[^\n]+.')
            c.expect('Are you sure you want to continue connecting \(yes/no/\[fingerprint\]\)?')
            c.sendline('yes')
        elif n == 2:
            sys.exit(1)
    c.sendline('our_password') # our password
    c.expect('/home/user # ')
    if args.command:
        c.sendline(args.command)
    if args.interact:
        c.logfile_read = None
        c.interact()
    else:
        # Note: expect() has default timeout = 30 seconds.
        c.expect('/home/user # ', timeout = None)
        # c.sendline('exit')
        # c.expect(f'Connection to {args.target_ip} closed.')
        c.wait()

if __name__ == '__main__':
    main()
