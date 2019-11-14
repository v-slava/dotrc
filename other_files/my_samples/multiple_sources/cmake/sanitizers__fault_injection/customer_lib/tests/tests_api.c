#include <stdlib.h>
#include <check.h>

#include "assert_wrappers.h"
#include "single_customer_fixture.h"
#include "customer.h"

START_TEST(create_customer___success___1)
{
}
END_TEST

START_TEST(create_customer___fail___lib_uninitialized)
{
    pCustomer customer_1 = NULL;
    init_customer_lib();
    fini_customer_lib();
    STATUS status = create_customer(&customer_1);
    assert_status(status, LIBRARY_IS_NOT_INITIALIZED);
}
END_TEST

START_TEST(set_get_customer_age)
{
    int new_age = 42;
    set_customer_age(customer, new_age);
    int updated_age = get_customer_age(customer);
    ck_assert_msg(updated_age == new_age, "Expected updated_age value is %d.\n\
Actual updated_age value is %d.", new_age, updated_age);
}
END_TEST

START_TEST(set_get_customer_name)
{
    const char* new_name = "Jason Hummer";
    STATUS status = set_customer_name(customer, new_name);
    assert_status(status, SUCCESS);
    const char* updated_name = get_customer_name(customer);
    ck_assert_msg(strcmp(updated_name, new_name) == 0,
"Expected updated_name value is %s.\nActual updated_name value is %s.",
new_name, updated_name);
}
END_TEST

START_TEST(customer_set_name___fail___name_is_too_long)
{
    enum {
        MAX_CUSTOMER_NAME_LEN = 50
    };
    char* new_name = malloc(MAX_CUSTOMER_NAME_LEN + 2);
    TEST_ASSERT(new_name != NULL);
    memset(new_name, 'a', MAX_CUSTOMER_NAME_LEN + 1);
    new_name[MAX_CUSTOMER_NAME_LEN + 1] = 0;
    STATUS status = set_customer_name(customer, new_name);
    assert_status(status, CUSTOMER_NAME_IS_TOO_LONG);
    free(new_name);
}
END_TEST

START_TEST(get_customer_data_buf_size)
{
    size_t size;
    STATUS status = get_customer_personal_data_buf_size(customer, &size);
    assert_status(status, SUCCESS);
    assert_size_t(size, 28, "personal data buf size");
}
END_TEST

START_TEST(get_customer_data_success)
{
    enum {
        BUF_SIZE = 100
    };
    char buf[BUF_SIZE];
    STATUS status = get_customer_personal_data(customer, buf, BUF_SIZE);
    assert_status(status, SUCCESS);
    assert_string(buf, "Age: 30. Name: unknown name", "personal data");
}
END_TEST

START_TEST(get_customer_data_insufficient_buf_size)
{
    enum {
        BUF_SIZE = 1
    };
    char buf[BUF_SIZE];
    STATUS status = get_customer_personal_data(customer, buf, BUF_SIZE);
    assert_status(status, INSUFFICIENT_BUFFER_SIZE);
}
END_TEST

START_TEST(get_customer_data_malloc_success)
{
    char *buf;
    STATUS status = get_customer_personal_data_malloc(customer, &buf);
    assert_status(status, SUCCESS);
    assert_string(buf, "Age: 30. Name: unknown name", "personal data");
    free(buf);
}
END_TEST

static void add_single_customer_fixture_test_cases(TCase* tcase)
{
    TEST_ASSERT(tcase != NULL);
    tcase_add_checked_fixture(tcase, single_customer_setup,
        single_customer_teardown);

    tcase_add_test(tcase, create_customer___success___1);
    tcase_add_test(tcase, set_get_customer_age);
    tcase_add_test(tcase, set_get_customer_name);
    tcase_add_test(tcase, customer_set_name___fail___name_is_too_long);
    tcase_add_test(tcase, get_customer_data_buf_size);
    tcase_add_test(tcase, get_customer_data_success);
    tcase_add_test(tcase, get_customer_data_insufficient_buf_size);
    tcase_add_test(tcase, get_customer_data_malloc_success);
}

static void add_non_fixture_test_cases(TCase* tcase)
{
    TEST_ASSERT(tcase != NULL);
    tcase_add_test(tcase, create_customer___fail___lib_uninitialized);
}

void add_test_cases_api(Suite* tsuite)
{
    TEST_ASSERT(tsuite != NULL);

    TCase *no_fixture_tcase = tcase_create("api no fixture");
    add_non_fixture_test_cases(no_fixture_tcase);
    suite_add_tcase(tsuite, no_fixture_tcase);

    TCase *single_customer_fixture_tcase = tcase_create("api single customer fixture");
    add_single_customer_fixture_test_cases(single_customer_fixture_tcase);
    suite_add_tcase(tsuite, single_customer_fixture_tcase);
}
