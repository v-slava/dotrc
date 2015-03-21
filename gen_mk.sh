#!/usr/bin/env bash

# Usage: ./gen_mk.sh > /tmp/Makefile

OS_SETTINGS=$PWD
HOME_SETTINGS=$OS_SETTINGS/home_settings
ROOT_SETTINGS=$OS_SETTINGS/root_settings
PATCHES_HOME_USER=$OS_SETTINGS/patches_home/$(id -un)

ROOT_SETTINGS_FILES=`find $ROOT_SETTINGS/ -type f`
HOME_SETTINGS_DIRS=`find $HOME_SETTINGS/ -type d`
HOME_SETTINGS_FILES=`find $HOME_SETTINGS/ -type f`
# Array of full file/dir names:
FILES_ROOT_TEMP=( $ROOT_SETTINGS_FILES )
FILES_HOME_TEMP=( $HOME_SETTINGS_FILES )
DIRS_HOME_TEMP=( $HOME_SETTINGS_DIRS )
# Array of file/dir names like '.vimrc', '.config/vimb/config':
FILES_ROOT=( ${FILES_ROOT_TEMP[@]##$ROOT_SETTINGS/} )
FILES_HOME=( ${FILES_HOME_TEMP[@]##$HOME_SETTINGS/} )
DIRS_HOME=( ${DIRS_HOME_TEMP[@]##$HOME_SETTINGS/} )

PATCHES_EXIST=
if [ -d $PATCHES_HOME_USER ]; then
	PATCHES_EXIST=true
	PATCHES_HOME_FILES=`find $PATCHES_HOME_USER/ -type f`
	PATCHES_HOME_TEMP=( $PATCHES_HOME_FILES )
	PATCHES_HOME=( ${PATCHES_HOME_TEMP[@]##$PATCHES_HOME_USER/} )
fi

# Write constant code to Makefile:
cat << EOF
.SUFFIXES:
MAKEFLAGS += --no-builtin-rules
export MAKEFLAGS

PHONY := help
help:
	@echo "Targets:\\n\\
update_home_timestamps\\n\\
home_settings\\n\\
update_root_timestamps\\n\\
root_settings"

FILES_HOME := ${FILES_HOME[@]}

FILES_ROOT := ${FILES_ROOT[@]}

DIRS_HOME := ${DIRS_HOME[@]}

MY_XDG_DIRS := \$(addprefix $HOME/.,data_xdg config_xdg cache_xdg)
\$(sort \$(MY_XDG_DIRS) \$(addprefix $HOME/,\$(DIRS_HOME))):
	mkdir -p \$@

$HOME/.runtime_xdg:
	mkdir -p \$@
	chmod 0700 \$@

$HOME/os_settings:
	ln -s $OS_SETTINGS $HOME/os_settings

HOME_TARGETS := \$(addprefix $HOME/,\$(FILES_HOME))
ROOT_TARGETS := \$(addprefix /,\$(FILES_ROOT))
HOME_FILES_OS_SETTINGS := \$(addprefix $HOME_SETTINGS/,\$(FILES_HOME))
ROOT_FILES_OS_SETTINGS := \$(addprefix $ROOT_SETTINGS/,\$(FILES_ROOT))

PHONY += home_settings
home_settings: \$(HOME_TARGETS) | \$(MY_XDG_DIRS) $HOME/.runtime_xdg $HOME/os_settings
	# xrdb ~/.Xresources

PHONY += root_settings
root_settings: \$(ROOT_TARGETS)
	# udpate-grub
	# /etc/init.d/udev restart
	# locale-gen
	# systemctl enable systemd-networkd.service
	# systemctl enable wpa_supplicant
	# systemctl set-default volkov.target
	# symlink: /etc/systemd/system/volkov.target.wants/startx@volkov.service -> ../startx@.service
	# symlink: /etc/systemd/system/noob.target.wants/startx@noob.service -> ../startx@.service

\$(ROOT_TARGETS): | check_root

PHONY += update_home_timestamps
update_home_timestamps:
	touch \$(HOME_FILES_OS_SETTINGS)

PHONY += update_root_timestamps
update_root_timestamps:
	touch \$(ROOT_FILES_OS_SETTINGS)

PHONY += check_root
check_root:
ifneq (\$(shell id -un),root)
	\$(error You must be root to execute this target)
endif

.PHONY: \$(PHONY)

EOF

# Generate per-file rules for all home files:
for file in "${FILES_HOME[@]}" ; do
	# A directory, which is order-only dependency for this file:
	dir_name=`dirname $HOME/$file`
	PATCH_FOUND=
	if [ -n "$PATCHES_EXIST" ]; then
		# Check whether there is a patch for this file_name:
		for patch_file in "${PATCHES_HOME[@]}" ; do
			if [ "$file" = "$patch_file" ]; then
				PATCH_FOUND=true
				break
			fi
		done
	fi
	if [ -n "$PATCH_FOUND" ]; then
		echo -e "$HOME/$file: $HOME_SETTINGS/$file $PATCHES_HOME_USER/$file | $dir_name\n\
	cp $< \$@\n\
	patch \$@ $PATCHES_HOME_USER/$file\n"
	else
		echo -e "$HOME/$file: $HOME_SETTINGS/$file | $dir_name\n\
	cp $< \$@\n"
	fi
done

# Generate per-file rules for all root files:
for file in "${FILES_ROOT[@]}" ; do
	echo -e "/$file: $ROOT_SETTINGS/$file\n\tcp $< \$@\n"
done

