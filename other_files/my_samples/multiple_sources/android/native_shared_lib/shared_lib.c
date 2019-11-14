#include <stdio.h>

int var = 34;

void func1()
{
    printf("Shared library: f1() is executed now. var = %d.\n", var);
    puts("Shared lib1");
    ++var;
    puts("Shared lib2");
    --var;
    puts("Shared lib3");
    puts("Shared lib4");
}
