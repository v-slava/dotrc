PHONY := all
all: download_git
	make -C progress CFLAGS="-O2 -flto"
	make -C progress -j 9
	su -c "make -C progress install"

PHONY += download_git
download_git:
	git clone https://github.com/Xfennec/progress
	# cd progress && git tag && git checkout v0.6

PHONY += clean
clean:
	rm -rf progress

.PHONY: $(PHONY)
