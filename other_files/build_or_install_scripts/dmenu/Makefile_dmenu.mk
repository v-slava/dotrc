OUT := $(PWD)/dmenu_out

# Use git repository:
DMENU_VERSION := dmenu
DMENU_COMMIT := 4.9
DMENU_URL := http://git.suckless.org/dmenu

# Use suckless tools website:
# DMENU_VERSION := dmenu-4.6
# DMENU_ARCHIVE_NAME := $(DMENU_VERSION).tar.gz
# DMENU_URL := dl.suckless.org/tools/$(DMENU_ARCHIVE_NAME)

CLIPBOARD_PATCH := $(DOTRC)/other_files/build_or_install_scripts/dmenu/dmenu_clipboard.patch

# Generate CLIPBOARD_PATCH (old method, no git):
# diff -urn dmenu_orig_folder dmenu_modified_folder > dmenu.diff
# Fix in vim (compare with old version).

PHONY := all
all: apply_patch install_dependencies
	make -C $(OUT)/$(DMENU_VERSION)
	su -c "make -C $(OUT)/$(DMENU_VERSION) install"

PHONY += install_dependencies
install_dependencies:
	su -c "apt-get install libxinerama-dev libx11-dev libxft-dev libfreetype6-dev libxmu-dev"

PHONY += apply_patch
apply_patch: $(OUT)/$(DMENU_VERSION) $(CLIPBOARD_PATCH)
	cd $(OUT)/$(DMENU_VERSION) && git apply $(CLIPBOARD_PATCH)
	# patch -N -d $< -p 1 < $(CLIPBOARD_PATCH)

$(OUT)/$(DMENU_VERSION): | $(OUT)
	cd $(OUT) && git clone $(DMENU_URL)
	cd $@ && git checkout $(DMENU_COMMIT)

# $(OUT)/$(DMENU_VERSION): $(OUT)/$(DMENU_ARCHIVE_NAME)
# 	tar xf $< -C $(OUT)

# $(OUT)/$(DMENU_ARCHIVE_NAME): | $(OUT)
# 	wget -P $| $(DMENU_URL)

PHONY += clean
clean:
	rm -rf $(OUT)

$(OUT):
	mkdir -p $@

.PHONY: $(PHONY)
