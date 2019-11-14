def generate_ninja():
    import ninja_syntax
    import string

    class FileContext(ninja_syntax.Writer):
        def __init__(self, output, width):
            ninja_syntax.Writer.__init__(self, output = output, width = width)
            # Map of variable name => expanded variable value.
            self.vars = {}

        def expand(self, str, local_vars={}):
            """Expand variables in a string."""
            return ninja_syntax.expand(str, self.vars, local_vars)

        def variable(self, key, value, indent = 0):
            self.vars[key] = self.expand(value)
            return super(FileContext, self).variable(key = key, value = value, indent = indent)

        def build_alias(self, alias_name, inputs):
            self.build(outputs = alias_name, rule = '', inputs = ['phony'] + inputs)

    def get_package_from_manifest(file_path):
        with open(file_path) as file:
            lines_list = file.readlines()
            for line in lines_list:
                import re
                m = re.search('[ \t]*package[ \t]*=[ \t]*"(.+?)"[ \t]*>?[ \t]*$', line)
                if m:
                    return m.group(1)

    def append_files_from_dir(context, output_list, ninja_dir):
        dir = context.expand(ninja_dir)
        import os
        for root_dir, sub_folders, file_names in os.walk(dir):
            for file_name in file_names:
                file_path = root_dir + '/' + file_name
                file_path = string.replace(file_path, dir, ninja_dir)
                list.append(output_list, file_path)

    f = FileContext(open('build.ninja', 'w'), 140)
    from configure import add_user_settings
    add_user_settings(f)
    f.newline()
    f.comment("Build tools from Android SDK:")
    f.variable('android_jar', "$android_sdk_root_dir/platforms/$platorm/android.jar")
    f.variable('aapt', "$android_sdk_root_dir/build-tools/$android_build_tools_version/aapt")
    f.variable('dx', "$android_sdk_root_dir/build-tools/$android_build_tools_version/dx")
    f.variable('zipalign', "$android_sdk_root_dir/build-tools/$android_build_tools_version/zipalign")
    f.variable('apksigner', "$android_sdk_root_dir/build-tools/$android_build_tools_version/apksigner")
    f.variable('adb', "$android_sdk_root_dir/platform-tools/adb")
    f.newline()
    f.comment("Project files and directories (input):")
    f.variable('manifest', "$prj_root_dir/AndroidManifest.xml")
    package = get_package_from_manifest(f.expand("$manifest"))
    package_path = '/' + string.replace(package, '.', '/')
    f.variable('resources_dir', "$prj_root_dir/res")
    f.variable('java_dir', "$prj_root_dir/java")
    f.newline()
    f.comment("Output files and directories:")
    f.variable('interm_dir', "$output_dir/intermediate")
    f.variable('class_dir', "$interm_dir/class")
    f.variable('R_java_dir', "$interm_dir/r_java")
    f.variable('R_java_file', "$R_java_dir" + package_path + "/R.java")
    R_java_classes = ['R$$attr.class', 'R$$id.class', 'R$$layout.class', 'R$$string.class', 'R.class']
    R_java_classes = ["$class_dir" + package_path + '/' + c for c in R_java_classes]
    # app_name = f.expand("$app_name")
    f.variable('dex_file_name', "classes.dex")
    f.variable('dex_file', "$interm_dir/$dex_file_name")
    f.variable('apk_resources', "$interm_dir/$app_name.apk.resources")
    f.variable('apk_unsigned_unaligned', "$interm_dir/$app_name.apk.unsigned.unaligned")
    f.variable('apk_unsigned_aligned', "$interm_dir/$app_name.apk.unsigned.aligned")
    f.variable('apk_file', "$output_dir/$app_name.apk")
    f.newline()
    f.comment("Command templates for rules:")
    f.variable('javac_template', "$javac -cp $android_jar$class_path_separator$class_dir -d $class_dir")
    f.comment("-f  force overwrite of existing files")
    f.comment("-M  specify full path to AndroidManifest.xml to include in zip")
    f.comment("-S  directory in which to find resources.  Multiple directories will be scanned")
    f.comment("    and the first match found (left to right) will take precedence.")
    f.comment("-I  add an existing package to base include set")

    f.variable('aapt_template', "$aapt package -f -M $manifest -S $resources_dir -I $android_jar") # -A "assets"
    f.newline()
    f.comment("Generate R.java file from XML resources (*.xml -> R.java):")
    f.comment("-m  make package directories under location specified by -J")
    f.comment("-J  specify where to output R.java resource constant definitions")
    f.rule(name = 'R_java_rule', command = "$aapt_template -m -J $R_java_dir",
           description = "AAPT AndroidManifest.xml res/* -> R.java")
    resource_dependencies = ["$manifest"]
    append_files_from_dir(f, resource_dependencies, "$resources_dir")
    f.build(outputs = ["$R_java_file"], rule = 'R_java_rule', inputs = resource_dependencies)
    f.newline()
    f.comment("Generate apk from XML resources (*.xml -> .apk.resources):")
    f.comment("-F  specify the apk file to output")
    f.rule(name = 'apk_resources_rule', command = "$aapt_template -F $out",
           description = "AAPT AndroidManifest.xml res/* -> .apk.resources")
    f.build(outputs = ["$apk_resources"], rule = 'apk_resources_rule', inputs = resource_dependencies)
    f.newline()
    f.comment("Compile resources (R.java -> *.class):")
    f.rule(name = 'R_java_classes_rule', command = "$javac_template $R_java_file", description = "JAVAC R.java (resources)")
    f.build(outputs = R_java_classes, rule = 'R_java_classes_rule', inputs = ["$R_java_file"])
    f.newline()
    f.comment("Compile java files (*.java -> *.class):")
    java_dir = f.expand("$java_dir")
    java_files = []
    class_files = [] + R_java_classes
    append_files_from_dir(f, java_files, "$java_dir")
    for file in java_files:
        relative_file = string.lstrip(file, "$java_dir/")
        rule_name = 'javac_rule__' + string.replace(string.rstrip(relative_file, ".java"), '/', '_')
        f.rule(name = rule_name, command = "$javac_template " + file, description = "JAVAC " + relative_file)
        class_file = "$class_dir/" + string.lstrip(string.rstrip(file, '.java'), '$java_dir/') + '.class'
        list.append(class_files, class_file)
        f.build(outputs = [class_file], rule = rule_name, inputs = ["$java_dir/" + relative_file] + R_java_classes)
    f.build_alias('classes', class_files)
    f.newline()
    f.comment('Convert class files to dex file (*.class -> classes.dex):')
    f.rule(name = 'dex_rule', command = "$dx --dex --no-optimize --num-threads=4 --incremental --output=$out $class_dir",
           description = "DX *.class -> classes.dex")
    f.build(outputs = ["$dex_file"], rule = 'dex_rule', inputs = class_files)
    f.build_alias('dex', ["$dex_file"])
    f.newline()
    f.comment("Generate unsigned and unaligned apk:")
    f.rule(name = 'unsigned_unaligned_apk_rule', command = "$multiple_commands_prefix cp $apk_resources $out \
    && cd $interm_dir && $aapt add -f $out $dex_file_name", description = "AAPT add: .apk.resources + .dex -> .apk.unsigned.unaligned")
    f.build(outputs = ["$apk_unsigned_unaligned"], rule = 'unsigned_unaligned_apk_rule', inputs = ["$apk_resources", "$dex_file"])
    f.newline()
    f.comment("Align apk:")
    f.rule(name = 'align_apk_rule', command = "$zipalign -f 4 $in $out",
           description = "ZIPALIGN .apk.unsigned.unaligned -> .apk.unsigned.aligned")
    f.build(outputs = ["$apk_unsigned_aligned"], rule = 'align_apk_rule', inputs = ["$apk_unsigned_unaligned"])
    f.newline()
    f.comment("Sign apk:")
    f.rule(name = 'apk_rule', command = "$apksigner sign $keystore_options --out $out $apk_unsigned_aligned",
           description = "APKSIGNER .apk.unsigned + .keystore -> .apk")
    f.build(outputs = ["$apk_file"], rule = 'apk_rule', inputs = ["$apk_unsigned_aligned", "$keystore_file"])
    f.build_alias('apk', ["$apk_file"])
    f.default("apk")

generate_ninja()
