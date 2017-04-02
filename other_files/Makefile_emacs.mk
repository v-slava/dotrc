VERSION := 24.5
FOLDER := emacs-$(VERSION)
ARCHIVE := $(FOLDER).tar.xz

PHONY := all
all: $(FOLDER)
	cd $< && ./configure --with-xpm=no --with-jpeg=no --with-gif=no --with-tiff=no
	make -C $< -j 9
	su -c "make -C $< install"

$(FOLDER): $(ARCHIVE)
	tar xf $<

$(ARCHIVE):
	wget http://ftp.gnu.org/gnu/emacs/$(ARCHIVE)

PHONY += clean
clean:
	rm -rf $(FOLDER) $(ARCHIVE)

.PHONY: $(PHONY)
