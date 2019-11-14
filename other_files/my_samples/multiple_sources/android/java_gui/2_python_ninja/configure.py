#!/usr/bin/python

def add_user_settings(f):
    f.comment("BEGIN_USER_SETTINGS")
    f.newline()
    f.comment("Android SDK. You don't need full Android Studio.")
    f.comment("Just command-line tools should be enough. To download, visit")
    f.comment("https://developer.android.com/studio/index.html")
    f.variable('android_sdk_root_dir', "/home/volkov/downloads/tools_r25.2.3-linux")
    f.variable('android_build_tools_version', "25.0.1")
    f.variable('platorm', "android-25")
    f.comment('Android requires .class files, compiled by javac 1.7 (see output of "javac -version").')
    f.comment("If you have javac 1.8, you need to set android_javac_options (see example below).")
    f.comment("If you have javac 1.7, you can set android_javac_options empty (or do not set at all).")
    f.variable('android_javac_options', "-source 1.7 -target 1.7 -bootclasspath /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar")
    f.comment("-g => generate all debugging info. -g:none => generate no debugging info.")
    f.variable('javac', "javac $android_javac_options -g")
    from os.path import expanduser
    home = expanduser("~")
    f.variable('keystore_file', home + "/.android/debug.keystore")
    f.variable('keystore_options', "--ks $keystore_file --ks-pass pass:android")
    f.comment("For linux set class_path_separator to ':', for windows - to ';'")
    f.variable('class_path_separator', ':')
    f.comment("For linux set multiple_commands_prefix to '' (or do not set at all), for windows - to 'cmd /c'")
    # f.variable('multiple_commands_prefix', 'cmd /c')
    f.newline()
    f.variable('app_name', 'tcp_data_transfer')
    f.comment('"AndroidManifest.xml" and "res" are expected here:')
    f.variable('prj_root_dir', "/home/volkov/other/my_samples/multiple_sources/android_java_gui/2_python_ninja/prj_root/src/main")
    f.newline()
    import os
    f.comment("A directory to write all output files into:")
    f.variable('output_dir', os.path.dirname(os.path.realpath(__file__)) + '/out')
    f.newline()
    f.comment("END_USER_SETTINGS")

import sys
# sys.path.insert(0, "/media/files/workspace/android_build_system")
import main
