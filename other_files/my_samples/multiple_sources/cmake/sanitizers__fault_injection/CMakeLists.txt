cmake_minimum_required(VERSION 2.8)
project(customer_app)

add_subdirectory(customer_lib/lib)

if (SANITIZE_OTHER STREQUAL "yes")
    set(SANITIZE_OTHER_FLAGS "-fsanitize=memory -fsanitize-memory-track-origins -fPIE -pie -fno-omit-frame-pointer -fsanitize=integer -fsanitize=undefined -g3")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${SANITIZE_OTHER_FLAGS}")
    set(CMAKE_C_COMPILER /usr/bin/clang)
    add_custom_target(run_customer_app
        CK_FORK=no MSAN_SYMBOLIZER_PATH=${LLVM_SYMBOLIZER}
        ./customer_app DEPENDS customer_app)
    target_compile_options(customer PRIVATE -fPIC)
endif ()

set(ROOT_DIR ${PROJECT_SOURCE_DIR})
set(SRC_DIR ${ROOT_DIR})
file(GLOB SRC_FILES ${SRC_DIR}/*.c)

add_executable(customer_app ${SRC_FILES})
target_compile_options(customer_app PRIVATE -Wall -Wextra)
target_link_libraries(customer_app PRIVATE customer)
target_include_directories(customer_app PRIVATE ${CUSTOMER_LIB_PUBLIC_INCLUDE_DIRS})
