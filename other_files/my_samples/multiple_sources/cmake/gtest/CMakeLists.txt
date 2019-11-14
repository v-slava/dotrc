cmake_minimum_required(VERSION 2.8)
project(gtest_example)

# Add Google Test
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB TESTS_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

set(UNIT_TESTS_EXECUTABLE gtests)
add_executable(${UNIT_TESTS_EXECUTABLE} ${TESTS_SRCS})

target_link_libraries(${UNIT_TESTS_EXECUTABLE} ${GTEST_LIBRARIES} pthread)
target_compile_options(${UNIT_TESTS_EXECUTABLE} PRIVATE -Wall -Wextra -pthread)

if (ANALYZE_CLANG STREQUAL "yes")
	set(CMAKE_C_COMPILER   /usr/share/clang/scan-build/ccc-analyzer)
	set(CMAKE_CXX_COMPILER /usr/share/clang/scan-build/c++-analyzer)
endif ()

if (USE_CLANG STREQUAL "yes")
	set(CMAKE_C_COMPILER   /usr/bin/clang)
	set(CMAKE_CXX_COMPILER /usr/bin/clang++)
endif ()

set(LLVM_SYMBOLIZER /usr/bin/llvm-symbolizer-3.5)

if (SANITIZE_ADDRESS STREQUAL "yes")
	set(SANITIZE_ADDRESS_FLAGS "-fsanitize=address -g3")
	set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   ${SANITIZE_ADDRESS_FLAGS}")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SANITIZE_ADDRESS_FLAGS}")
	set(CMAKE_C_COMPILER /usr/bin/clang)
	set(CMAKE_CXX_COMPILER /usr/bin/clang++)
	add_custom_target(run_tests
		ASAN_SYMBOLIZER_PATH=${LLVM_SYMBOLIZER}
		./${UNIT_TESTS_EXECUTABLE} DEPENDS ${UNIT_TESTS_EXECUTABLE}
	)
endif ()

add_custom_target(run_tests ${PROJECT_BINARY_DIR}/${UNIT_TESTS_EXECUTABLE}
	DEPENDS ${UNIT_TESTS_EXECUTABLE}
)

set(VALGRIND_FLAGS --leak-check=full --track-origins=yes --show-leak-kinds=all)
add_custom_target(analyze_valgrind valgrind ${VALGRIND_FLAGS}
	./${UNIT_TESTS_EXECUTABLE} DEPENDS ${UNIT_TESTS_EXECUTABLE})

if (COVERAGE STREQUAL "yes")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --coverage")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage")
	add_custom_target(coverage
		find -name "*.o" | xargs gcov
&& echo && echo "See *.gcov files for details. Uncovered code pieces are marked with #####"
		DEPENDS run_tests)
endif ()

