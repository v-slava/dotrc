#!/usr/bin/python3

import os, sys, subprocess

connected_sh = os.environ['DOTRC'] + '/other_files/bluetooth_connected.sh'
ret = subprocess.run([connected_sh])
if ret.returncode != 0:
    print('Error: bluetooth headset is not connected', file = sys.stderr)
    sys.exit(1)

my_env = os.environ.copy()
my_env['LANG'] = 'C'
ret = subprocess.run(['pactl', 'list', 'cards'], check = True, env = my_env,
                capture_output = True, encoding = 'ascii')

bluetooth_card = None
profiles = []
profiles_started = False
active_profile = None
for line in ret.stdout.splitlines():
    # print(f'|{line}|')
    # continue
    if not bluetooth_card:
        prefix = '\tName: '
        if line.startswith(f'{prefix}bluez_card.'):
            bluetooth_card = line[len(prefix):]
        continue
    if not profiles_started:
        if line == '\tProfiles:':
            profiles_started = True
        continue
    prefix = '\tActive Profile: '
    if line.startswith(prefix):
        active_profile = line[len(prefix):]
        break
    prefix = '\t\t'
    assert(line.startswith(prefix))
    end = line.find(':')
    assert(end > 0)
    profile = line[len(prefix):end]
    profiles.append(profile)

assert(len(profiles) == 3)
for profile in profiles:
    assert(profile in ['headset_head_unit', 'a2dp_sink', 'off'])
profiles.remove('off')
profiles.remove(active_profile)
new_profile = profiles[0]
subprocess.run(['pactl', 'set-card-profile', bluetooth_card, new_profile],
        check = True)
