mv /etc/X11/xorg.conf /etc/X11/xorg.conf_native

Normally linux guest under virtualbox shouldn't have xorg.conf.
That's why /etc/X11/virtual/xorg.conf should be a broken symlink.

To fix brightness control on ASUS laptop, make sure the following
exists in /etc/X11/xorg.conf_native ("Option" is optional, if
"Driver" is "modesetting" - change to "intel"):

Section "Device"
        Identifier  "Card0"
        Driver      "intel"
        Option      "Backlight"  "intel_backlight"
EndSection
