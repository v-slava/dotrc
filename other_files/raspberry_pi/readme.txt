apt-get install xserver-xorg-video-fbturbo xserver-xorg-input-libinput xinit rxvt-unicode-256color libpam-systemd

$ crontab -e
# m h  dom mon dow   command
  0 4   *   *   *      /home/pi/upgrade.sh
