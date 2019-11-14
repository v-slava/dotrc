#!/bin/bash

#DEVICE=emulator-5554
DEVICE=000e828d10071e

# Exit immediately on error
set -e

# Build binary for debugging
$NDK_gcc app.c -o app.out # -mthumb # -march=armv7-a

# Create objdump
#arm-linux-androideabi-objdump -D -z app.out > objdump_app

# Copy files to remote Android device
adb -s $DEVICE push ./app.out $REMOTE_PATH/
adb -s $DEVICE push $NDK_ROOT/prebuilt/android-arm/gdbserver/gdbserver $REMOTE_PATH/

#adb -s $DEVICE shell "$REMOTE_PATH/app.out" ; exit 0

# Launch gdbserver
adb -s $DEVICE shell $REMOTE_PATH/gdbserver :5039 $REMOTE_PATH/app.out &

# Launch arm-linux-androideabi-gdb
run_in_new_terminal.sh --geometry 80x51 adb -s $DEVICE \
"forward tcp:5039 tcp:5039 && sleep 1 && arm-linux-androideabi-gdb -x ./gdb_commands"

# Delete some files
wait
echo "Deleting all..."
rm ./app.out
adb -s $DEVICE shell "rm $REMOTE_PATH/app.out $REMOTE_PATH/gdbserver"
