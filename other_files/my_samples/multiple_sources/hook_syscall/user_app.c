#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

static inline void error_exit(const char* func)
{
    perror(func);
    exit(EXIT_FAILURE);
}

int main(int argc, char* argv[])
{
    int fd = open(argv[0], 0, O_RDONLY);
    if (fd == -1)
        error_exit("open");
    if (close(fd) == -1)
        error_exit("close");
    return EXIT_SUCCESS;
}
