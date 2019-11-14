#!/bin/bash

set -e

rm -rf prj_root
mkdir prj_root
cd prj_root

mkdir -p src/main/java/org/hello
mkdir -p src/main/res/values
mkdir -p src/main/res/layout

cat << EOF > src/main/AndroidManifest.xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="org.hello"
    android:versionCode="1"
    android:versionName="1.0.0" >

    <application android:label="@string/app_name" >
        <activity
            android:name=".HelloActivity"
            android:label="@string/app_name" >
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
    </application>

</manifest>
EOF

cat << EOF > src/main/res/values/strings.xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <string name="app_name">Android Gradle</string>
</resources>
EOF

cat << EOF > src/main/res/layout/hello_layout.xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:orientation="vertical"
    android:layout_width="fill_parent"
    android:layout_height="fill_parent"
    >
<TextView
    android:id="@+id/text_view"
    android:layout_width="fill_parent"
    android:layout_height="wrap_content"
    />
</LinearLayout>
EOF

cat << EOF > src/main/java/org/hello/HelloActivity.java
package org.hello;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class HelloActivity extends Activity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.hello_layout);
    }

    @Override
    public void onStart() {
        super.onStart();
        TextView textView = (TextView) findViewById(R.id.text_view);
        textView.setText("Hello world!");
    }

}
EOF

cat << EOF > build.gradle
buildscript {
    repositories {
        jcenter()
    }

    dependencies {
        classpath 'com.android.tools.build:gradle:2.2.2'
    }
}

apply plugin: 'com.android.application'

android {
    compileSdkVersion 25
    buildToolsVersion "25.0.1"

    // dexOptions {
    //     incremental true
    // }
}
EOF

cat << EOF > local.properties
sdk.dir=/home/volkov/downloads/tools_r25.2.3-linux
EOF

cat << EOF > gradle.properties
org.gradle.daemon=true
org.gradle.parallel=true
org.gradle.jvmargs=-Xmx1536m
org.gradle.configureondemand=true
android.enableBuildCache=true
EOF

set -ex
# gradle build
# gradle assembleDebug --offline

OUT=../out
rm -rf $OUT
mkdir $OUT
cd $OUT
ln -s ../configure.py configure.py
./configure.py
cat build.ninja
ninja
