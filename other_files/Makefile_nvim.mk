PHONY := all
all: download_git install_packages
	make -C neovim CMAKE_BUILD_TYPE=Release -j 9
	su -c "make -C neovim install"

PHONY += install_packages
install_packages:
	su -c "apt-get install cmake libtool-bin automake gettext pkg-config"

PHONY += download_git
download_git:
	git clone https://github.com/neovim/neovim
	cd neovim && git tag

PHONY += clean
clean:
	rm -rf neovim

