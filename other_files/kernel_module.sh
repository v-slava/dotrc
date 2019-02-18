#!/usr/bin/env bash

set -e
OUT=out
FILE=some-file-name
DRV_NAME=some_driver_name
SCRIPT=/tmp/on_device_script.sh

if [ ! -d $OUT ]; then
    mkdir $OUT
    cd $OUT
    ln -s ../*.c .
    cat << EOF > Makefile
obj-m += $FILE.o

all:
	+make -C \$(KDIR) M=\$(PWD) modules

clean:
	+make -C \$(KDIR) M=\$(PWD) clean
EOF
    cd -
fi

source $DOTRC_S/other_files/kernel_env.sh
export KDIR=$KDIR
cd $OUT
emacsclient --eval '(my-save-all-buffers)'
make -j5
# make -j5 clean
scpto.sh $FILE.ko
cat << EOF > $SCRIPT
if lsmod | grep -q $DRV_NAME ; then
    stderr=\`rmmod $DRV_NAME 2>&1\`
    RET=\$?
    if [ \$RET -ne 0 ] || [ -n "\$stderr" ]; then
        echo -e \$stderr 1>&2
        if [ \$RET -eq 0 ]; then
            RET=1
        fi
        exit \$RET
    fi
fi
insmod /tmp/$FILE.ko
EOF
scpto.sh $SCRIPT
shd "chmod a+x $SCRIPT && $SCRIPT"

# tail -f /var/log/messages | grep 'kern\.'
