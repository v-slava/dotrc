#!/bin/bash

set -ex

# systemctl --user status check_new_email.timer
# systemctl --user status check_new_email.service
systemctl --user stop check_new_email.timer

while pgrep evolution 1>/dev/null ; do
    pkill evolution
    sleep 0.1
done
