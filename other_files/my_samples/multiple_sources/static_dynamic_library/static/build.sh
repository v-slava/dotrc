#!/bin/bash

# Build static library:
gcc -c my_lib1.c -o my_lib1.o
gcc -c my_lib2.c -o my_lib2.o
ar rcs my_lib.a my_lib1.o my_lib2.o # r - insert files with replacement; c - create new archive; v - verbose; s - Add an index to the archive, or update it if it already exists.

gcc -c main.c -o main.o
gcc main.o my_lib.a -o a.out

./a.out
echo $?

rm *.o *.a *.out

