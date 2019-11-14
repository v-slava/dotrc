cmake_minimum_required(VERSION 2.8)

add_subdirectory(lib_unity)

set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
file(GLOB SRC_FILES ${ROOT_DIR}/src/*.c)

add_executable(tests_customer_lib ${SRC_FILES})
target_compile_options(tests_customer_lib PRIVATE -Wall -Wextra)
target_link_libraries(tests_customer_lib PRIVATE unity)
target_include_directories(tests_customer_lib PRIVATE ${UNITY_PUBLIC_INCLUDE_DIRS})

add_custom_target(run_tests_customer_lib ./tests_customer_lib DEPENDS tests_customer_lib)

