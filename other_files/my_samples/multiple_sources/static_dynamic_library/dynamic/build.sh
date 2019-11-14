#!/bin/bash

# Build shared library:
gcc -c -fPIC my_lib1.c -o my_lib1.o
gcc -c -fPIC my_lib2.c -o my_lib2.o
gcc -shared -o libmy_lib.so my_lib1.o my_lib2.o # -lc (libc.so)
gcc -c main.c -o main.o

#gcc main.o -o a.out -lmy_lib -L.
#export LD_LIBRARY_PATH=.

gcc main.o -o a.out -lmy_lib -L. -Wl,-rpath=. # insert a directory to search for libmy_lib.so into "a.out" (rpath)

./a.out
echo $?

rm *.o *.so *.out

