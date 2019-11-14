#ifndef SINGLE_CUSTOMER_FIXTURE_H
#define SINGLE_CUSTOMER_FIXTURE_H

#include "customer.h"

static pCustomer customer;

static void single_customer_setup(void)
{
    customer = NULL;
    init_customer_lib();
    STATUS status = create_customer(&customer);
    assert_status(status, SUCCESS);
    ck_assert_msg(customer != NULL, "create_customer() returned NULL pointer");
}

static void single_customer_teardown(void)
{
    destroy_customer(customer);
    fini_customer_lib();
}

#endif // SINGLE_CUSTOMER_FIXTURE_H
