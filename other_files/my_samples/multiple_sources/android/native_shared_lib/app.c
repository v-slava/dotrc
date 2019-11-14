#include <stdio.h>
#include "lib_header.h"

int main()
{
    printf("variable from shared library: var = %d. Call f1():\n", var);
    func1();
    puts("Done!");
    return 0;
}
