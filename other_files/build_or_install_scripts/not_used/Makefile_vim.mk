PHONY := all
all: download_git install_dependencies
	cd vim && CFLAGS="-O2" LDFLAGS="-O2" ./configure --enable-fail-if-missing --disable-darwin --disable-smack --disable-selinux --disable-xsmp --disable-xsmp-interact --enable-luainterp=no --enable-perlinterp=no --enable-pythoninterp=yes --enable-python3interp=yes --enable-tclinterp=no --enable-rubyinterp=no --enable-cscope --disable-netbeans --enable-multibyte --enable-gui=gtk3 --with-x
	make -C vim -j 9
	su -c "make -C vim install"

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install python-dev python3-dev'

PHONY += download_git
download_git:
	git clone https://github.com/vim/vim
	cd vim && git tag

PHONY += clean
clean:
	rm -rf vim

