#!/bin/bash

DEVICE=emulator-5554
#DEVICE=000e828d10071e

# Exit immediately on error
set -e

$NDK_gcc ./app.s -o ./app.out
#arm-linux-androideabi-as -v -march=armv5te -mfloat-abi=soft -mfpu=vfp -meabi=5 --noexecstack -o app.o ./app.s
#arm-linux-androideabi-ld -lc --sysroot=$SYSROOT $SYSROOT/usr/lib/crtbegin_dynamic.o $SYSROOT/usr/lib/crtend_android.o -dynamic-linker /system/bin/linker app.o -o app.out

adb -s $DEVICE push ./app.out $REMOTE_PATH/

#arm-linux-androideabi-objdump -D -z app.out > objdump_app

#adb -s $DEVICE shell "$REMOTE_PATH/app.out" ; exit 0

# Launch gdbserver
adb -s $DEVICE push $NDK_ROOT/prebuilt/android-arm/gdbserver/gdbserver $REMOTE_PATH/
adb -s $DEVICE shell $REMOTE_PATH/gdbserver :5039 $REMOTE_PATH/app.out &

# Launch arm-linux-androideabi-gdb
run_in_new_terminal.sh --geometry 80x51 adb -s $DEVICE \
"forward tcp:5039 tcp:5039 && sleep 1 && arm-linux-androideabi-gdb -x ./gdb_commands"

# Delete some files
wait
echo "Deleting all..."
rm ./app.out
adb -s $DEVICE shell "rm $REMOTE_PATH/app.out $REMOTE_PATH/gdbserver"
