#!/usr/bin/env bash

# File: sound_en.dsl.files.zip - compressed wav/mp3 files, no folder allowed.
# File: sound_en.dsl. Format example (use tabs before [s]):
#
# #NAME "Sound (En)"
# #INDEX_LANGUAGE "English"
# #CONTENTS_LANGUAGE "English"
#
# mother
# 	[s]mother.mp3[/s]
# feather
# 	[s]feather.mp3[/s]

set -e

DICT_NAME=sound_en
RESOURCE_DIR=$PWD/res_dir
FILE_EXT=mp3
DSL_TAG=s

echo "Creating $DICT_NAME.dsl ..."

cat << EOF > $DICT_NAME.dsl
#NAME "$DICT_NAME"
#INDEX_LANGUAGE "English"
#CONTENTS_LANGUAGE "English"

EOF

all_files=$(ls $RESOURCE_DIR)
for file in $all_files ; do
	echo -e "${file%%.${FILE_EXT}}\n\t[$DSL_TAG]$file[/$DSL_TAG]" >> $DICT_NAME.dsl
done

echo "Compiling $DICT_NAME.dsl into $DICT_NAME.dsl.dz ..."
dictzip $DICT_NAME.dsl

echo "Compressing resources ..."
CUR_DIR=$PWD
cd $RESOURCE_DIR
zip $CUR_DIR/$DICT_NAME.dsl.files.zip *

echo "Done!"
