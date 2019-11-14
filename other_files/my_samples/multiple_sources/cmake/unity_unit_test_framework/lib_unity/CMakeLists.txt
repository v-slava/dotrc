cmake_minimum_required(VERSION 2.8)
project(Unity)

# set(ROOT_DIR .)
# set(ROOT_DIR ${PROJECT_SOURCE_DIR})
set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Unity)

set(DIR_1 ${ROOT_DIR}/src)
set(DIR_2 ${ROOT_DIR}/extras/fixture/src)
set(PUBLIC_INCLUDE_DIRS ${DIR_1} ${DIR_2})
file(GLOB SRC_FILES ${DIR_1}/*.c ${DIR_2}/*.c)

add_library(unity STATIC ${SRC_FILES})
target_include_directories(unity PRIVATE ${PUBLIC_INCLUDE_DIRS})
target_compile_options(unity PRIVATE -Wall -Wextra)

get_directory_property(hasParent PARENT_DIRECTORY)
if(hasParent)
    # export to top-level CMakeLists.txt:
    set(UNITY_PUBLIC_INCLUDE_DIRS ${PUBLIC_INCLUDE_DIRS} PARENT_SCOPE)
    # set(UNITY_SRC_DIRS ${PUBLIC_INCLUDE_DIRS} PARENT_SCOPE)
    # set(UNITY_SRC_FILES ${SRC_FILES} PARENT_SCOPE)
endif()
