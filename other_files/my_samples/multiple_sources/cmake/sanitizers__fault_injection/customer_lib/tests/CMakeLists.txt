cmake_minimum_required(VERSION 2.8)

set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
file(GLOB SRC_FILES ${ROOT_DIR}/*.c)
add_executable(tests_customer_lib ${SRC_FILES})
target_compile_options(tests_customer_lib PRIVATE -Wall -Wextra)

if (TESTS_COVERAGE STREQUAL "yes")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --coverage")
    add_custom_target(tests_coverage_customer_lib
    gcov ./lib/CMakeFiles/customer.dir/src/customer.c.o
    && cat ./customer.c.gcov DEPENDS run_tests_customer_lib)
endif ()

if (ANALYZE_CLANG STREQUAL "yes")
    set(CMAKE_C_COMPILER /usr/share/clang/scan-build-3.5/ccc-analyzer)
endif ()

set(LLVM_SYMBOLIZER /usr/bin/llvm-symbolizer-3.5)

if (SANITIZE_ADDRESS STREQUAL "yes")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address -g3")
    set(CMAKE_C_COMPILER /usr/bin/clang)
    add_custom_target(run_tests_customer_lib
        CK_FORK=no ASAN_SYMBOLIZER_PATH=${LLVM_SYMBOLIZER}
        ./tests_customer_lib DEPENDS tests_customer_lib)
endif ()

if (SANITIZE_OTHER STREQUAL "yes")
    set(SANITIZE_OTHER_FLAGS "-fsanitize=memory -fsanitize-memory-track-origins -fPIE -pie -fno-omit-frame-pointer -fsanitize=integer -fsanitize=undefined -g3")
    # -fsanitize=thread
    # -fsanitize=cfi # control flow integrity. Requires -flto.
    # -fsanitize=stack # clang 3.5 doesn't support this
    # -fsanitize=dataflow # link errors (symbol not found)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${SANITIZE_OTHER_FLAGS}")
    set(CMAKE_C_COMPILER /usr/bin/clang)
    add_custom_target(run_tests_customer_lib
        CK_FORK=no MSAN_SYMBOLIZER_PATH=${LLVM_SYMBOLIZER}
        ./tests_customer_lib DEPENDS tests_customer_lib)
    # check unit test framework should be configured as follows:
    # CFLAGS="-fPIC -O2" ./configure
    find_library(CHECK check /usr/local/lib)
else ()
    set(CHECK check)
endif ()

add_subdirectory(../lib lib EXCLUDE_FROM_ALL)
target_compile_options(customer PRIVATE -fPIC)
target_include_directories(tests_customer_lib PRIVATE ${CUSTOMER_LIB_PUBLIC_INCLUDE_DIRS})
target_link_libraries(tests_customer_lib PRIVATE customer ${CHECK} rt m)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")

if (ANALYZE_DUMA STREQUAL "yes")
    target_link_libraries(tests_customer_lib PRIVATE duma)
endif ()

if (USE_FIU STREQUAL "yes")
    target_compile_definitions(tests_customer_lib PRIVATE FIU_ENABLE)
    target_compile_definitions(customer PRIVATE FIU_ENABLE)
    target_link_libraries(tests_customer_lib PRIVATE fiu dl)
    add_custom_target(run_tests_customer_lib fiu-run -x ./tests_customer_lib
        DEPENDS tests_customer_lib)
else ()
    if (NOT SANITIZE_ADDRESS STREQUAL "yes" AND NOT SANITIZE_OTHER STREQUAL "yes")
        add_custom_target(run_tests_customer_lib ./tests_customer_lib
            DEPENDS tests_customer_lib)
    endif ()
endif ()

set(VALGRIND_FLAGS --leak-check=full --track-origins=yes)
add_custom_target(tests_analyze_valgrind CK_FORK=no valgrind ${VALGRIND_FLAGS}
    ./tests_customer_lib DEPENDS tests_customer_lib)

# ninja -v
# make VERBOSE=1
# set(CMAKE_VERBOSE_MAKEFILE "ON")

# set(CMAKE_EXE_LINKER_FLAGS "-static")
# set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -flto")

# debug tests:
# CK_FORK=no gdb lib/test_customer_lib
