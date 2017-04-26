PHONY := all
all: download_git
	CFLAGS="-O2 -flto" LDFLAGS="-O2 -flto" make -C vimb -j 4
	su -c "make -C vimb install"

PHONY += download_git
download_git:
	git clone https://github.com/fanglingsu/vimb.git
	cd vimb && git tag && git checkout 2.11

PHONY += clean
clean:
	rm -rf vimb

.PHONY: $(PHONY)