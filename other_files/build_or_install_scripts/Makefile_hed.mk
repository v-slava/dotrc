PHONY := all
all: download_git
	su -c "apt-get install libncurses5-dev"
	make -C hed -j9
	su -c "make -C hed install"

PHONY += download_git
download_git:
	git clone git://repo.or.cz/hed.git
	# cd hed && git tag && git checkout hed-0.5

PHONY += clean
clean:
	rm -rf hed

.PHONY: $(PHONY)
