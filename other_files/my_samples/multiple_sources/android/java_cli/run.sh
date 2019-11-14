#!/bin/bash

DX=/home/user/other_files/adt-bundle-linux-x86-20130522/sdk/build-tools/android-4.2.2/dx
REMOTE_PATH=/data/local/tmp

javac hello_world.java
$DX --dex --output=classes.dex hello_world.class
zip hello_world.zip classes.dex

adb push hello_world.zip $REMOTE_PATH/
adb shell mkdir $REMOTE_PATH/dalvik-cache
adb shell ANDROID_DATA=$REMOTE_PATH dalvikvm -cp $REMOTE_PATH/hello_world.zip hello_world
