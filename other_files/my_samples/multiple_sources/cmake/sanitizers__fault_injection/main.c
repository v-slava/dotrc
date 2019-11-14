#include <stdio.h>
#include <stdlib.h>
#include "customer.h"
#include "status.h"

int main(void)
{
    pCustomer customer_1;
    STATUS status;

    init_customer_lib();
    HANDLE_ERROR(error, status, create_customer(&customer_1));

    printf("Default customer's age = %d\n", get_customer_age(customer_1));
    set_customer_age(customer_1, 25);
    printf("Updated customer's age = %d\n", get_customer_age(customer_1));

    printf("Default customer's name = %s\n", get_customer_name(customer_1));
    HANDLE_ERROR(error, status, set_customer_name(customer_1, "John the Kick"));
    printf("Updated customer's name = %s\n", get_customer_name(customer_1));

    size_t buf_size_required;
    HANDLE_ERROR(error, status, get_customer_personal_data_buf_size(customer_1,
        &buf_size_required));
    printf("buf_size_required = %zu\n", buf_size_required);
    enum
    {
        BUF_SIZE = 29
    };
    char buf[BUF_SIZE];
    HANDLE_ERROR(error, status, get_customer_personal_data(customer_1, buf,
        BUF_SIZE));
    printf("Customer's personal data 1: %s\n", buf);

    char* buf_2;
    HANDLE_ERROR(error, status, get_customer_personal_data_malloc(customer_1,
        &buf_2));
    printf("Customer's personal data 2: %s\n", buf_2);
    free(buf_2);

    destroy_customer(customer_1);

    puts("\nAll has been done successfully!");
    fini_customer_lib();
    return EXIT_SUCCESS;

error:
    printf("The following error occured: %s.\n", get_message(status));
    fini_customer_lib();
    return EXIT_FAILURE;
}
