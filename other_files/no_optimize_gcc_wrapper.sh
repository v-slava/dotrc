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
# # USE_DEFAULT_O0_OPIMIZATIONS=true
# test -z "$DOTRC" && DOTRC=/media/files/workspace/dotrc
# . $DOTRC/other_files/no_optimize_gcc_wrapper.sh

LOG=/tmp/gcc_wrapper_log

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
    echo "$ORIG_GCC $@" >> $LOG
    exec "$ORIG_GCC" "$@"
fi

if [ "$USE_DEFAULT_O0_OPIMIZATIONS" != "true" ]; then
    FLAGS="$("$ORIG_GCC" --help=optimizers -Q -O0 | grep '\[enabled\]' \
        | cut -d' ' -f3 | grep -v '^\-fno-' | grep -v '^\-frtti$' \
        | grep -v '^\-fomit-frame-pointer$' | cut -c 3-)"
    FLAGS_REMOVE="-f${FLAGS//$'\n'/ -f}"
    FLAGS_ADD="-fno-${FLAGS//$'\n'/ -fno-}"
fi

for arg do
    shift
    case "$arg" in
        "-fomit-frame-pointer" | \
        "-fno-omit-frame-pointer" | \
        "-O0" | "-O" | "-O1" | "-O2" | "-O3" | "-Os" |"-Ofast" | "-Og" | \
        "-g" | "-g1" | "-g2" | "-g3")
            ;;
        *)
            if [ "$USE_DEFAULT_O0_OPIMIZATIONS" != "true" ]; then
                SKIP=false
                for flag in $FLAGS_REMOVE ; do
                    if [ "$flag" = "$arg" ]; then
                        SKIP=true
                        break
                    fi
                done
                if [ "$SKIP" = "true" ]; then
                    continue
                fi
            fi
            set -- "$@" "$arg"
            ;;
    esac
done

ARGS=(
    -g3
    -fno-omit-frame-pointer
)

if [ "$USE_DEFAULT_O0_OPIMIZATIONS" != "true" ]; then
    ARGS+=($FLAGS_ADD)
fi

ARGS+=(
)

echo "$ORIG_GCC $@ ${ARGS[@]}" >> $LOG
exec $ORIG_GCC "$@" "${ARGS[@]}"
