#!/bin/bash

set -ex

PRJ_ROOT_DIR=$PWD
OUT_DIR=~/error_handling_check_all
CMAKE_COMMON_FLAGS=" -G Ninja "

if [ ! -d "$OUT_DIR" ]; then
    mkdir "$OUT_DIR"
fi

function config_build_run {
    if [ $# -eq 0 ]; then
        echo "Wrong config_build_run() arguments" 1>&2
        exit 1
    fi
    DIR="$1"
    shift 1
    if [ ! -d "$DIR" ]; then
        mkdir "$DIR"
        cd "$DIR"
        cmake $CMAKE_COMMON_FLAGS "$@" "$PRJ_ROOT_DIR"
    fi
    cd "$DIR"
    # Build tests + clang static analyzis:
    ninja -j9 tests_customer_lib
    # Run unit tests:
    ninja run_tests_customer_lib
}

config_build_run "$OUT_DIR/main" -DCMAKE_BUILD_TYPE=Debug -DTESTS_COVERAGE=yes \
    -DUSE_FIU=yes -DANALYZE_CLANG=yes
ninja tests_coverage_customer_lib

config_build_run "$OUT_DIR/valgrind" -DCMAKE_BUILD_TYPE=Debug
ninja tests_analyze_valgrind

config_build_run "$OUT_DIR/sanitize_address" -DCMAKE_BUILD_TYPE=Release \
    -DSANITIZE_ADDRESS=yes

config_build_run "$OUT_DIR/duma" -DCMAKE_BUILD_TYPE=Debug -DANALYZE_DUMA=yes

config_build_run "$OUT_DIR/sanitize_other" -DCMAKE_BUILD_TYPE=Release \
    -DSANITIZE_OTHER=yes
