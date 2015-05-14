PHONY := all
all: download_git
	make -C cv CFLAGS="-O2 -flto"
	make -C cv -j 9
	su -c "make -C cv install"

PHONY += download_git
download_git:
	git clone https://github.com/Xfennec/cv.git
	# cd cv && git tag && git checkout v0.6

PHONY += clean
clean:
	rm -rf cv

.PHONY: $(PHONY)
