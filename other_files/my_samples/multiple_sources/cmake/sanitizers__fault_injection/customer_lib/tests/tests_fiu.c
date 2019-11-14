#ifdef FIU_ENABLE

#include <stdarg.h>
#include <stdlib.h>
#include <check.h>
#include <fiu.h>
#include <fiu-control.h>

#include "assert_wrappers.h"
#include "single_customer_fixture.h"
#include "customer.h"

#include <stdio.h>

START_TEST(create_customer___fail___no_free_customers)
{
    pCustomer customer;
    init_customer_lib();
    TEST_ASSERT(fiu_enable("no_free_customers", 1, NULL, FIU_ONETIME) == 0);
    STATUS status = create_customer(&customer);
    assert_status(status, CREATE_CUSTOMER_FAILED);
}
END_TEST

START_TEST(get_data_buf_size___fail___snprintf_failed)
{
    TEST_ASSERT(fiu_enable("snprintf_failed", 1, NULL, FIU_ONETIME) == 0);
    size_t size;
    STATUS status = get_customer_personal_data_buf_size(customer, &size);
    assert_status(status, SNPRINTF_FAILED);
}
END_TEST

START_TEST(get_personal_data___fail___malloc_failed)
{
    TEST_ASSERT(fiu_enable("libc/mm/malloc", 1, NULL, FIU_ONETIME) == 0);
    char* buf;
    STATUS status = get_customer_personal_data_malloc(customer, &buf);
    assert_status(status, MALLOC_FAILED);
}
END_TEST

static unsigned int fail_call_num;
static int fail_snprintf(const char* name UNUSED, int* failnum UNUSED,
    void** failinfo UNUSED, unsigned int* flags UNUSED)
{
    TEST_ASSERT(fail_call_num != 0);
    --fail_call_num;
    if (fail_call_num == 0)
        return 1;
    return 0;
}

START_TEST(get_personal_data_malloc___fail___snprintf_failed_2)
{
    fail_call_num = 2;
    TEST_ASSERT(fiu_enable_external("snprintf_failed", 1, NULL, FIU_ONETIME,
        fail_snprintf) == 0);
    char* buf;
    STATUS status = get_customer_personal_data_malloc(customer, &buf);
    assert_status(status, SNPRINTF_FAILED);
}
END_TEST

static void add_single_customer_fixture_test_cases(TCase* tcase)
{
    TEST_ASSERT(tcase != NULL);
    tcase_add_checked_fixture(tcase, single_customer_setup,
        single_customer_teardown);

    tcase_add_test(tcase, get_data_buf_size___fail___snprintf_failed);
    tcase_add_test(tcase, get_personal_data___fail___malloc_failed);
    tcase_add_test(tcase, get_personal_data_malloc___fail___snprintf_failed_2);
}

static void add_non_fixture_test_cases(TCase* tcase)
{
    TEST_ASSERT(tcase != NULL);
    tcase_add_test(tcase, create_customer___fail___no_free_customers);
}

void add_test_cases_fiu(Suite* tsuite)
{
    TEST_ASSERT(tsuite != NULL);
    fiu_init(0);

    TCase *no_fixture_tcase = tcase_create("fiu no fixture");
    add_non_fixture_test_cases(no_fixture_tcase);
    suite_add_tcase(tsuite, no_fixture_tcase);

    TCase *single_customer_fixture_tcase = tcase_create("fiu single customer fixture");
    add_single_customer_fixture_test_cases(single_customer_fixture_tcase);
    suite_add_tcase(tsuite, single_customer_fixture_tcase);
}

#endif // FIU_ENABLE
