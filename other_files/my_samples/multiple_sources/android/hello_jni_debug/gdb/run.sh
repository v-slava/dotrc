#!/bin/bash

ANT=/home/user/other_files/adt-bundle-linux-x86-20130522/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin/ant
ANDROID=/home/user/other_files/adt-bundle-linux-x86-20130522/sdk/tools/android
NDK_BUILD=/home/user/other_files/android-ndk-r8e/ndk-build
ADB=/home/user/other_files/adt-bundle-linux-x86-20130522/sdk/platform-tools/adb


NDK=/home/user/desktop/other_files/android-ndk-r8e
TOOLCHAIN=arm-linux-androideabi-4.7
PLATFORM=android-9
PREFIX=$NDK/toolchains/$TOOLCHAIN/prebuilt/linux-x86/bin/arm-linux-androideabi

cd ..

#rm -rf hello-jni
#cp -r hello-jni-copy hello-jni

$ADB uninstall com.example.hellojni

$PREFIX-gcc ./gdb/gdbserver.c -o ./gdb/gdbserver --sysroot=$NDK/platforms/$PLATFORM/arch-arm/
cp ./gdb/gdbserver ./hello-jni/libs/armeabi/

cd hello-jni
#$ANDROID list target
#echo -e "\n\nandroid update:\n\n"
#$ANDROID update project -p . -s --target android-10 # 17
#echo -e "\n\nNDK_BUILD:\n\n"
#$NDK_BUILD
echo -e "\n\nant debug:\n\n"
$ANT debug

$ADB install bin/HelloJni-debug.apk

#$ADB shell "cat /data/local/tmp/gdb_log"

#$ADB push bin/HelloJni-debug.apk /storage/sdcard0/
