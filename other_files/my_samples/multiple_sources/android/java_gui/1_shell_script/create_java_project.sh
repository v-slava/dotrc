#!/bin/bash

APP_NAME=my_app
PACKAGE_NAME=com.example.${APP_NAME}
ACTIVITY_NAME=${APP_NAME}_activity

WORKSPACE=.
PRJ_ROOT=$WORKSPACE/$APP_NAME

# ANDROID=/home/v_volkov/other_files/adt-bundle-linux-x86/sdk/tools/android
ANDROID=/home/volkov/downloads/extracted_tools_r25.2.3-linux.zip/tools/android
ANT=/home/v_volkov/other_files/adt-bundle-linux-x86/eclipse/plugins/org.apache.ant/bin/ant

set -e
if [[ -d $PRJ_ROOT ]]; then
	rm -rf $PRJ_ROOT
fi
# adb uninstall $PACKAGE_NAME

echo -e "Creating project..\n"
$ANDROID create project --target 1 --name $APP_NAME --path $PRJ_ROOT --activity $ACTIVITY_NAME --package $PACKAGE_NAME

if [[  true ]]; then
	cp ./my_files/main.xml ./$APP_NAME/res/layout
	cp ./my_files/strings.xml ./$APP_NAME/res/values/
	cp ./my_files/AndroidManifest.xml ./$APP_NAME/
	cp ./my_files/my_app_activity.java ./$APP_NAME/src/com/example/my_app/
fi

echo -e "\nBuilding project..\n"
cd $PRJ_ROOT
$ANT debug
#$ANT release

# adb install ./bin/$APP_NAME-debug.apk

