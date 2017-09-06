PHONY := all
all: download_git install_dependencies
	mkdir rtags_out
	cd rtags_out && cmake -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Release ../rtags
	ninja -C rtags_out -j 9
	su -c "ninja -C rtags_out install"

PHONY += download_git
download_git:
	git clone --recursive https://github.com/Andersbakken/rtags.git
	cd rtags && git tag # && git checkout 'v2.14'

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install -t jessie-backports cmake ninja-build libclang-3.8-dev llvm-3.8-dev'

PHONY += clean
clean:
	rm -rf rtags rtags_out

.PHONY: $(PHONY)
