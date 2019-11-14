#!/bin/bash

DEVICE=emulator-5554
#DEVICE=000e828d10071e

OUT=$PWD/out

# Exit immediately on error
set -e

if [[ -d $OUT ]]; then
	rm -rf $OUT
fi
mkdir $OUT

# Build shared library:
$NDK_gcc -g shared_lib.c -o $OUT/libmy_shared_lib.so -shared -fPIC -lc # (libc.so)

#gcc main.o -o a.out -lmy_lib -L.
#export LD_LIBRARY_PATH=.

# Build binary
$NDK_gcc -g app.c -o $OUT/app.out -lmy_shared_lib -L$OUT

# Create objdumps
arm-linux-androideabi-objdump -D -z $OUT/app.out > $OUT/objdump_app
arm-linux-androideabi-objdump -D -z $OUT/libmy_shared_lib.so > $OUT/objdump_my_shared_lib

# Copy files to remote Android device
adb -s $DEVICE push $OUT/app.out $REMOTE_PATH/
adb -s $DEVICE push $OUT/libmy_shared_lib.so $REMOTE_PATH/
adb -s $DEVICE push $NDK_ROOT/prebuilt/android-arm/gdbserver/gdbserver $REMOTE_PATH/

#adb -s $DEVICE shell "cd $REMOTE_PATH && export LD_LIBRARY_PATH=. && ./app.out" ; exit 0

# Launch gdbserver
adb -s $DEVICE shell "cd $REMOTE_PATH && export LD_LIBRARY_PATH=. && ./gdbserver :5039 $REMOTE_PATH/app.out" &

# Launch arm-linux-androideabi-gdb
run_in_new_terminal.sh --geometry 80x51 adb -s $DEVICE \
"forward tcp:5039 tcp:5039 && sleep 1 && arm-linux-androideabi-gdb -x ./gdb_commands"

# Delete some files
wait
echo "Deleting remote files..."
adb -s $DEVICE shell "rm $REMOTE_PATH/*"

