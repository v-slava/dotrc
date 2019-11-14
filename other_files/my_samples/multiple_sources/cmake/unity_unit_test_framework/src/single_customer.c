#include "unity_fixture.h"

TEST_GROUP(single_customer);

int var1;

TEST_SETUP(single_customer)
{
	// this is run before each test
	var1 = 0x5a5a;
}

TEST_TEAR_DOWN(single_customer)
{
	var1 = 0;
}

TEST(single_customer, first_test_success)
{
	TEST_ASSERT_EQUAL(0x5a5a, var1);
}

TEST(single_customer, ignored_test)
{
	TEST_IGNORE_MESSAGE("This test was ignored");
	/* TEST_IGNORE(); */
}

TEST_GROUP_RUNNER(single_customer)
{
	RUN_TEST_CASE(single_customer, first_test_success);
	RUN_TEST_CASE(single_customer, ignored_test);
}
