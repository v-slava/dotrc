#ifndef ASSERT_WRAPPERS_H
#define ASSERT_WRAPPERS_H

#include "status.h"

#include <string.h>
#include <check.h>

#include <assert.h>
#define TEST_ASSERT assert
#define INLINE inline
#define UNUSED __attribute__((unused))

static INLINE void assert_status(STATUS actual, STATUS expected)
{
    ck_assert_msg(actual == expected,
"\nExpected status is %d with message: %s.\n\
Actual   status is %d with message: %s.\n", expected, get_message(expected),
    actual, get_message(actual));
}

static INLINE void assert_string(const char* actual, const char* expected,
    const char* name)
{
    TEST_ASSERT(actual != NULL);
    TEST_ASSERT(expected != NULL);
    TEST_ASSERT(name != NULL);
    ck_assert_msg(strcmp(actual, expected) == 0, "\nExpected %s is \"%s\".\n\
Actual   %s is \"%s\".\n", name, expected, name, actual);
}

static INLINE void assert_size_t(size_t actual, size_t expected,
    const char* name)
{
    TEST_ASSERT(name != NULL);
    ck_assert_msg(actual == expected, "\nExpected %s is %zu.\n\
Actual   %s is %zu.\n", name, expected, name, actual);
}

static INLINE void test_TODO(const char* file, unsigned int line)
{
    ck_assert_msg(0, "\nTODO unit test not implemented: %s:%d\n", file, line);
}
#define TEST_TODO test_TODO(__FILE__, __LINE__)

#endif // ASSERT_WRAPPERS_H
