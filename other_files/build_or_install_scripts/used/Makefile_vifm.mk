PHONY := all
all: download_git install_dependencies
	cd vifm && ./configure --disable-desktop-files --without-gtk CFLAGS="-O2 -flto" LDFLAGS="-O2 -flto"
	make -C vifm -j 9
	su -c "make -C vifm install"

# If your autotools version is too old downgrade it in vifm sources
# (for example recursively replace "1.15" to "1.14"). For example:
# $ cd vifm
# $ find -type f -print0 | xargs -0 sed -e 's/1\.15/1\.14/g' -i

PHONY += install_dependencies
install_dependencies:
	su -c "apt-get install libncursesw5-dev automake"

PHONY += download_git
download_git:
	git clone https://github.com/vifm/vifm
	cd vifm && git tag && git checkout v0.10 # need master (> v0.8.2)

PHONY += clean
clean:
	rm -rf vifm

.PHONY: $(PHONY)
