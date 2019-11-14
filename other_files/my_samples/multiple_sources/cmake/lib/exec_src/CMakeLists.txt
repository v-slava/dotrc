# Make sure the compiler can find include files from our Hello library.
include_directories (${cmake_demo_SOURCE_DIR}/exec_src)
include_directories (${cmake_demo_SOURCE_DIR}/include)

# Make sure the linker can find the Hello library once it is built.
link_directories (${cmake_demo_BINARY_DIR}/lib_src)

# Add executable called "demo_exec" that is built from the source files
# "main.cpp" and "file1.cxx". The extensions are automatically found.
add_executable (demo_exec main.cpp file1.cxx)

# Link the executable to the Hello library.
target_link_libraries (demo_exec static_lib shared_lib)
