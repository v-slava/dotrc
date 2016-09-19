mv /etc/X11/xorg.conf /etc/X11/xorg.conf_native

Normally linux guest under virtualbox shouldn't have xorg.conf.
That's why /etc/X11/virtual/xorg.conf should be a broken symlink.

