#include <stdio.h>
#include <stdlib.h>
#include <execinfo.h>

static void dump_stack(const char *message)
{
    void *trace[16];
    int max_frames = sizeof(trace) / sizeof(trace[0]);
    int num_frames = backtrace(trace, max_frames);
    const char *truncated = (num_frames == max_frames) ? ", truncated?" : "";
    char **symbols = backtrace_symbols(trace, num_frames);
    if (symbols == NULL)
    {
        printf("Failed to get stack trace (%s)", message);
        return;
    }
    printf("stack trace begin (%s%s)>>>", message, truncated);
    for (int frame = 0; frame < num_frames; frame++)
        printf("> %s", symbols[frame]);
    printf("stack trace end (%s)", message);
    free(symbols);
}
