#include <stdio.h>

const char* fname = "/data/local/tmp/gdb_log";

int main(int argc, char** argv)
{
    FILE* f = fopen(fname,"a+");
    int i = 0;

    if(!f)
    {
        perror("Can't open file");
        return -1;
    }
    fputs("= = =\n",f);
    for(i = 0;i < argc; ++i)
    {
        fputs(argv[i],f);
        fputs("\n",f);
    }

    fclose(f);

    return 0;
}
