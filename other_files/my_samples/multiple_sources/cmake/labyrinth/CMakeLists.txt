# cmake options: -DCMAKE_BUILD_TYPE=Debug

project(labyrinth)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

find_package(PkgConfig REQUIRED)

pkg_check_modules(GTK3 REQUIRED gtk+-3.0)
include_directories(${GTK3_INCLUDE_DIRS})
link_directories(${GTK3_LIBRARY_DIRS})
add_definitions(${GTK3_CFLAGS_OTHER})
target_link_libraries(${PROJECT_NAME} ${GTK3_LIBRARIES})

if (WIN32)
    set(GTK_BUNDLE_LIB_DIR D:/programming/gtk+-bundle_3.6.4-20130921_win32/lib)
    message(STATUS "Add GTK_BUNDLE_LIB_DIR link directory for windows: " ${GTK_BUNDLE_LIB_DIR})
    target_link_libraries(${PROJECT_NAME} -L${GTK_BUNDLE_LIB_DIR})
endif (WIN32)
