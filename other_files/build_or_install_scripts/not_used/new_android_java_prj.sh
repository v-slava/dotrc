#!/bin/bash

APP_NAME=my_app
PACKAGE_NAME=com.example.${APP_NAME}
ACTIVITY_NAME=${APP_NAME}_activity

WORKSPACE=/home/user/workspace
PRJ_ROOT=$WORKSPACE/$APP_NAME

ANDROID=/home/user/other_files/adt-bundle-linux-x86-20130522/sdk/tools/android
ANT=/home/user/other_files/adt-bundle-linux-x86-20130522/eclipse/plugins/org.apache.ant_1.8.3.v201301120609/bin/ant

set -e
if [[ -d $PRJ_ROOT ]]; then
	rm -rf $PRJ_ROOT
fi

echo -e "Creating project..\n"
$ANDROID create project --target 1 --name $APP_NAME --path $PRJ_ROOT --activity $ACTIVITY_NAME --package $PACKAGE_NAME

echo -e "\nBuilding project..\n"

cd $PRJ_ROOT
$ANT debug

adb install ./bin/$APP_NAME-debug.apk
adb uninstall $PACKAGE_NAME
