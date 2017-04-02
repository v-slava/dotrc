PHONY := all
all: download_git install_dependencies
	mkdir rtags_out
	cd rtags_out && cmake -DCMAKE_BUILD_TYPE=Release ../rtags
	make -C rtags_out -j 9
	su -c "make -C rtags_out install"

PHONY += download_git
download_git:
	git clone --recursive https://github.com/Andersbakken/rtags.git
	cd rtags && git tag # && git checkout 'v2.9'

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install cmake libncurses5-dev libssl-dev libclang-dev llvm-dev'

PHONY += clean
clean:
	rm -rf rtags

.PHONY: $(PHONY)
