#!/bin/bash

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
CMD_WITHOUT_COMPILER="$(echo $CMD | cut -d' ' -f 2-)"

set -e

# Clang static analysis:
scan-build-3.8 $CMD -Wall -Werror

# Clang-based linter:
clang-tidy-3.8 $CMD_WITHOUT_COMPILER

# Sanitizers:
$CMD -fsanitize=memory -fsanitize-memory-track-origins -g -O0 -fPIE -pie -fno-omit-frame-pointer -fno-optimize-sibling-calls
PATH=/usr/lib/llvm-3.8/bin $OUT

$CMD -fsanitize=address -g -O0 -fno-common -fno-omit-frame-pointer -fno-optimize-sibling-calls
PATH=/usr/lib/llvm-3.8/bin ASAN_OPTIONS=detect_stack_use_after_scope=1 $OUT

$CMD -fsanitize=undefined -g -O0 -fno-omit-frame-pointer
PATH=/usr/lib/llvm-3.8/bin UBSAN_OPTIONS=print_stacktrace=1 $OUT

# Valgrind:
$CMD -g
valgrind --error-exitcode=1 --leak-check=full --track-origins=yes $OUT

echo -e "\nNo issues found."
