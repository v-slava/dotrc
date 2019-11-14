#ifndef STATUS_H
#define STATUS_H

#define STATUS_LIST \
    X(SUCCESS, "Success") \
    X(LIBRARY_IS_NOT_INITIALIZED, "customer library is not initialized") \
    X(CREATE_CUSTOMER_FAILED, "create_customer() failed") \
    X(INSUFFICIENT_BUFFER_SIZE, "Insufficient buffer size") \
    X(SNPRINTF_FAILED, "snprintf() failed") \
    X(MALLOC_FAILED, "malloc() failed") \
    X(CUSTOMER_NAME_IS_TOO_LONG, "Passed customer name is too long") \
    /* do not add statuses after this line */ \
    X(GENERIC_ERROR, "Generic error") \
    X(TO_BE_IMPLEMENTED, "Not implemented (TODO)") \
    X(STATUS_RESERVED, "")

#define X(enum_entry, message) enum_entry,
typedef enum
{
    STATUS_LIST
} STATUS;
#undef X

#define SUCCEEDED(status) ((status) == SUCCESS)
#define FAILED(status) ((status) != SUCCESS)

#define RETURN_STATUS(status) return (status)

// #define TRY(status)
#define RETURN_ON_ERROR(status) \
    do { \
        STATUS _s = (status); \
        if (FAILED(_s)) { \
            RETURN_STATUS(_s); \
        } \
    } while (0)

#define BREAK_ON_ERROR(status) \
    if (FAILED(status)) { \
        break; \
    } \

#define GOTO_ON_ERROR(label, status) \
    do { \
        if (FAILED(status)) { \
            goto label; \
        } \
    } while (0)

#define HANDLE_ERROR(label, variable, status) \
    do { \
        variable = (status); \
        GOTO_ON_ERROR(label, variable); \
    } while (0)

const char* get_message(STATUS status);

#endif // #ifndef STATUS_H
