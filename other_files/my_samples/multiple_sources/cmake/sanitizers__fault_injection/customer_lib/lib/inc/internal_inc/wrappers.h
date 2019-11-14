#ifndef WRAPPERS_H
#define WRAPPERS_H

#include <stdlib.h>

#include "definitions.h"

static inline STATUS malloc_wrapper(void** buf, size_t size)
{
    ASSERT(buf != NULL);
    ASSERT(size != 0);

    void* local_buf = malloc(size);
    if (local_buf == NULL) {
        RETURN_STATUS(MALLOC_FAILED);
    }

    *buf = local_buf;
    RETURN_STATUS(SUCCESS);
}

#endif // WRAPPERS_H
