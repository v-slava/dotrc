#!/bin/bash

# The following script can be used to replace gcc in order to keep several files
# unoptimized. Create gcc wrapper like:

# #!/bin/bash
#
# ORIG_GCC=gcc
#
# NO_OPTIMIZE_FILES=(
#     "dir1/file1.c"
#     "/home/user/file2.cpp"
#     "file3.c"
#     "./file4.c"
# )
#
# . $DOTRC/other_files/no_optimize_gcc_wrapper.sh

if ! [[ "$(declare -p NO_OPTIMIZE_FILES)" =~ "declare -a" ]]; then
    echo "NO_OPTIMIZE_FILES env variable is not a valid bash array" 1>&2
    exit 1
fi

if [ ! -x "$ORIG_GCC" ] && [ ! -x "$(which "$ORIG_GCC")" ]; then
    echo "ORIG_GCC env variable doesn't point to valid executable file" 1>&2
    exit 1
fi

PROCESS=false
for file in "${NO_OPTIMIZE_FILES[@]}" ; do
    if [[ "$@" == *$file* ]] ; then
        PROCESS=true
        break
    fi
done

if [ "$PROCESS" != true ]; then
    exec $ORIG_GCC "$@"
fi

for arg do
    shift
    case "$arg" in
        "-fomit-frame-pointer" | \
        "-O1" | "-O2" | "-O3" | "-Os" |"-Ofast" | "-Og" | "-g" | "-g3")
            ;;
        *)
            set -- "$@" "$arg"
            ;;
    esac
done

exec $ORIG_GCC "$@ -g3 -fno-omit-frame-pointer|"
