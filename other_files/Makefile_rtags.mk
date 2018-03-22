PHONY := all
all: download_git install_dependencies
	mkdir rtags_out
	cd rtags_out && cmake -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Release ../rtags
	ninja -C rtags_out
	su -c "ninja -C rtags_out install"

PHONY += download_git
download_git:
	git clone --recursive https://github.com/Andersbakken/rtags.git
	cd rtags && git tag # && git checkout 'v2.14'

PHONY += install_dependencies
install_dependencies:
	su -c 'apt-get install cmake ninja-build libclang-6.0-dev llvm-6.0-dev clang-6.0'

PHONY += clean
clean:
	rm -rf rtags rtags_out

.PHONY: $(PHONY)
