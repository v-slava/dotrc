1) https://github.com/SrKomodo/shadowfox-updater/releases
./shadowfox_linux_x64 -profile-index 1 -set-dark-theme
# currently works: v2.2.0

2) Use firefox's built-in black theme.

3) Install addons:
https://addons.mozilla.org/en-US/firefox/addon/darkreader/?src=search
https://addons.mozilla.org/en-US/firefox/addon/vimium-ff/?src=search
https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/?src=search

# ==============================================================================
# The following settings are currently not used:

# To use my ./chrome/*.css add the following to $DOTRC/apply_home_settings:

FIREFOX_PROFILE=$(realpath ~/.mozilla/firefox/*.default-esr)
FIREFOX_SRC=$DOTRC/other_files/firefox/chrome
FIREFOX_DEST=$FIREFOX_PROFILE/chrome
if [ ! -e $FIREFOX_DEST ]; then
    ln -s $FIREFOX_SRC $FIREFOX_DEST
fi
if [ "$(realpath $FIREFOX_DEST)" != "$FIREFOX_SRC" ]; then
    echo "Failed to set $FIREFOX_DEST symlink" 1>&2
    exit 1
fi

# Additionally open about:config and set:
toolkit.legacyUserProfileCustomizations.stylesheets = true
