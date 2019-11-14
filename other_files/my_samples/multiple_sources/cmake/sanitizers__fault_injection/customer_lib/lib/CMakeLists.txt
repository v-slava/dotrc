cmake_minimum_required(VERSION 2.8)
project(customer_lib)

# set(ROOT_DIR .)
# set(ROOT_DIR ${PROJECT_SOURCE_DIR})
set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

set(SRC_DIR ${ROOT_DIR}/src)
file(GLOB SRC_FILES
    ${SRC_DIR}/*.c
)
add_library(customer STATIC ${SRC_FILES})
set(PUBLIC_INCLUDE_DIRS
    ${ROOT_DIR}/inc
)
set(PRIVATE_INCLUDE_DIRS
    ${ROOT_DIR}/inc/internal_inc
)
target_include_directories(customer PRIVATE
    ${PUBLIC_INCLUDE_DIRS} ${PRIVATE_INCLUDE_DIRS})

target_compile_options(customer PRIVATE -Wall -Wextra)
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_definitions(customer PRIVATE DEBUG_PRINT)
else ()
    target_compile_definitions(customer PRIVATE NO_PRINTS)
endif ()

get_directory_property(hasParent PARENT_DIRECTORY)
if(hasParent)
    # export to top-level CMakeLists.txt:
    set(CUSTOMER_LIB_PUBLIC_INCLUDE_DIRS ${PUBLIC_INCLUDE_DIRS} PARENT_SCOPE)
    # set(CUSTOMER_LIB_PRIVATE_INCLUDE_DIRS ${PRIVATE_INCLUDE_DIRS} PARENT_SCOPE)
    # set(CUSTOMER_LIB_SRC_DIRS ${SRC_DIR} PARENT_SCOPE)
    # set(CUSTOMER_LIB_SRC_FILES ${SRC_FILES} PARENT_SCOPE)
endif()
