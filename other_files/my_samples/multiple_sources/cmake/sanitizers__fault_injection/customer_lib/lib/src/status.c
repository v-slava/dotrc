#include "status.h"
#include "definitions.h"

#define X(code, message) message,
static const char* statusMessages[STATUS_RESERVED + 1] = {
    STATUS_LIST
};
#undef X

const char* get_message(STATUS status) {
    ASSERT(status >= 0);
    ASSERT(status < STATUS_RESERVED);
    return statusMessages[status];
}
