PHONY := all
all: download_git install_dependencies
	cd kakoune/src && debug=no make -j 5 all man
	su -c "debug=no make -C kakoune/src install"

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install --install-recommends libncursesw5-dev asciidoc'

PHONY += download_git
download_git:
	git clone https://github.com/mawww/kakoune/

# cd kakoune && git tag

PHONY += clean
clean:
	rm -rf kakoune

