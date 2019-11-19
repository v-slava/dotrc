VERSION := 25.3
FOLDER := emacs-$(VERSION)
ARCHIVE := $(FOLDER).tar.xz

PHONY := all
all: $(FOLDER) install_dependencies
	cd $< && ./configure --with-xpm=yes --with-jpeg=no --with-gif=no --with-tiff=no
	make -C $< -j 9
	su -c "make -C $< install"

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install libgtk-3-dev libxpm-dev'

$(FOLDER): $(ARCHIVE)
	tar xf $<

$(ARCHIVE):
	wget http://ftp.gnu.org/gnu/emacs/$(ARCHIVE)

PHONY += clean
clean:
	rm -rf $(FOLDER) $(ARCHIVE)

.PHONY: $(PHONY)
