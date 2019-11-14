#!/bin/bash

# See also static analysis program for C: splint
# Formal verification (checks assertions): cbmc

if [ $# -lt 2 ]; then
    echo -e "Usage: $(basename $0) OUT_FILE CMD\n\
\n\
For example:\n\
$(basename $0) /tmp/main.out gcc ~/main.c -o /tmp/main.out"
    exit 1
fi

OUT="$1"
shift
CMD="$@"

# Clang static analysis:
scan-build $CMD -Wall -Werror

# Clang-based linter:
TMP_DIR=/tmp/check_program
rm -rf $TMP_DIR
mkdir -p $TMP_DIR
bear -o $TMP_DIR/compile_commands.json $CMD
SRCS=$(grep '        "file": "[^.]\+\.c"$' $TMP_DIR/compile_commands.json \
    | cut -c 18- | rev | cut -c 2- | rev)
clang-tidy -warnings-as-errors=* -p $TMP_DIR $SRCS
rm -rf $TMP_DIR

LLVM_PATH=$(realpath $(which clang))

# Sanitizers:
$CMD -fsanitize=memory -fsanitize-memory-track-origins -g -O0 -fPIE -pie -fno-omit-frame-pointer -fno-optimize-sibling-calls
PATH=$LLVM_PATH $OUT

$CMD -fsanitize=address -g -O0 -fno-common -fno-omit-frame-pointer -fno-optimize-sibling-calls
PATH=$LLVM_PATH ASAN_OPTIONS=detect_stack_use_after_scope=1 $OUT

$CMD -fsanitize=undefined -g -O0 -fno-omit-frame-pointer
PATH=$LLVM_PATH UBSAN_OPTIONS=print_stacktrace=1 $OUT

# Valgrind:
$CMD -g
valgrind --error-exitcode=1 --leak-check=full --track-origins=yes $OUT

echo -e "\nNo issues found."

# See also (static annotation of thread-safety properties):
# https://gcc.gnu.org/wiki/ThreadSafetyAnnotation
# http://clang.llvm.org/docs/ThreadSafetyAnalysis.html
