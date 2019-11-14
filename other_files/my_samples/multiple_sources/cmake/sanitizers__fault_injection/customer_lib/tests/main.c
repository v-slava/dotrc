#include <stdlib.h>
#include <check.h>
#include "assert_wrappers.h"

/*
START_TEST(some_test)
{
    const char* currency = "EUR";
    ck_assert_msg(strcmp(currency, "USD") == 0,
"Expected value of currency is |%s|, however actual value is |%s|\n", "USD",
currency);
    ck_assert_msg(strcmp(currency, "USD") == 0, NULL);
    ck_assert_str_eq(currency, "USD");
    ck_assert_int_eq(0, 1);
    ck_assert(0 == 1);
}
END_TEST
*/

int main(void)
{
    Suite *customer_tsuite = suite_create("libcustomer test suite");

    void add_test_cases_api(Suite* tsuite);
    add_test_cases_api(customer_tsuite);

#ifdef FIU_ENABLE
    void add_test_cases_fiu(Suite* tsuite);
    add_test_cases_fiu(customer_tsuite);
#endif // FIU_ENABLE

    SRunner *runner = srunner_create(customer_tsuite);
    /* srunner_set_fork_status(runner, CK_NOFORK); */
    srunner_run_all(runner, CK_NORMAL);
    int num_failed = srunner_ntests_failed(runner);
    srunner_free(runner);

    return (num_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
