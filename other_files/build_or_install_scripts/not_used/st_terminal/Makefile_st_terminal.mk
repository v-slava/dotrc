PHONY := all
all: patch_src
	make -C st
	su -c "make -C st install"

PHONY += patch_src
patch_src: download_git
	cd st && patch -p1 < $DOTRC/other_files/st_terminal.patch

PHONY += download_git
download_git:
	git clone http://git.suckless.org/st
	cd st && git tag && git checkout 0.8.2

PHONY += clean
clean:
	rm -rf st

.PHONY: $(PHONY)
