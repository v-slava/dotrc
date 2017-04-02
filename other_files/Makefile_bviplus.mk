VERSION := 0.9.4

PHONY := all
all: bviplus-$(VERSION)
	make -C $^
	su -c "make -C $^ install"

PHONY += extract
extract: bviplus-$(VERSION)

bviplus-$(VERSION): bviplus-$(VERSION).tgz
	tar xf $^

PHONY += download
download: bviplus-$(VERSION).tgz

bviplus-$(VERSION).tgz:
	wget http://downloads.sourceforge.net/project/bviplus/bviplus/$(VERSION)/bviplus-$(VERSION).tgz

PHONY += clean
clean:
	rm -rf bviplus-$(VERSION) bviplus-$(VERSION).tgz

.PHONY: $(PHONY)
