#include <iostream>

#include "file1.h"
#include "interface.h"

int main()
{
    file1_f1();
    static_lib_f1();
    static_lib_c_f2();
    shared_lib_f1();
    std::cout << "Done!" << std::endl;
    return 0;
}
