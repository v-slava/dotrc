#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "customer.h"
#include "definitions.h"
#include "wrappers.h"
#include "fiu-local.h"

// Customer implementation:
enum {
    MAX_CUSTOMER_NAME_LEN = 50
};

struct Customer
{
    int age;
    char name[MAX_CUSTOMER_NAME_LEN + 1];
    // something other
};

enum {
    MAX_CUSTOMERS = 10
};
static struct Customer all_customers[MAX_CUSTOMERS];
// array of pointers to unused (free) customers:
static pCustomer free_customers[MAX_CUSTOMERS];

size_t customer_index;
bool library_is_initialized;

static INLINE void construct_customer(pCustomer customer)
{
    ASSERT(customer != NULL);
    enum {
        DEFAULT_AGE = 30
    };
    set_customer_age(customer, DEFAULT_AGE);
    const char* default_name = "unknown name";
    STATUS status UNUSED = set_customer_name(customer, default_name);
    ASSERT(SUCCEEDED(status));
}

static INLINE STATUS snprintf_personal_data(const pCustomer customer, char* str,
    size_t size, int* len)
{
    ASSERT(customer != NULL);
    ASSERT(len != NULL);
    int ret = snprintf(str, size, "Age: %d. Name: %s", customer->age,
        customer->name);
    fiu_do_on("snprintf_failed", if (ret >= 0) { ret = -1; });
    if (ret < 0) {
        RETURN_STATUS(SNPRINTF_FAILED);
    }
    *len = ret;
    RETURN_STATUS(SUCCESS);
}

// assume that we have buffer long enough
static INLINE STATUS write_customer_personal_data(const pCustomer customer,
    char* buf, size_t buf_size)
{
    ASSERT(customer != NULL);
    ASSERT(buf != NULL);
    int len UNUSED;
    RETURN_ON_ERROR(snprintf_personal_data(customer, buf, buf_size, &len));
    ASSERT(len >= 0);
    ASSERT((size_t)len < buf_size);
    RETURN_STATUS(SUCCESS);
}

void init_customer_lib(void)
{
    size_t i;
    for (i = 0; i < MAX_CUSTOMERS; ++i) {
        free_customers[i] = &all_customers[i];
    }
    customer_index = MAX_CUSTOMERS - 1;
    library_is_initialized = true;
}

void fini_customer_lib(void)
{
    library_is_initialized = false;
}

STATUS create_customer(pCustomer* customer)
{
    ASSERT(customer != NULL);
    if (!library_is_initialized) {
        RETURN_STATUS(LIBRARY_IS_NOT_INITIALIZED);
    }
    fiu_do_on("no_free_customers", customer_index = 0);
    if (customer_index == 0) {
        RETURN_STATUS(CREATE_CUSTOMER_FAILED);
    }
    --customer_index;
    pCustomer cur_customer = free_customers[customer_index];
    construct_customer(cur_customer);
    *customer = cur_customer;
    RETURN_STATUS(SUCCESS);
}

void destroy_customer(pCustomer customer)
{
    ASSERT(customer != NULL);
    ASSERT(customer >= &all_customers[0]);
    ASSERT(customer < &all_customers[MAX_CUSTOMERS]);
    ASSERT(customer_index < MAX_CUSTOMERS);
    free_customers[customer_index] = customer;
    ++customer_index;
}

int get_customer_age(const pCustomer customer)
{
    ASSERT(customer != NULL);
    return customer->age;
}

void set_customer_age(pCustomer customer, int age)
{
    ASSERT(customer != NULL);
    customer->age = age;
}

const char* get_customer_name(const pCustomer customer)
{
    ASSERT(customer != NULL);
    return customer->name;
}

STATUS set_customer_name(pCustomer customer, const char* name)
{
    ASSERT(customer != NULL);
    ASSERT(name != NULL);
    size_t name_len = strlen(name);
    if (name_len > MAX_CUSTOMER_NAME_LEN) {
        RETURN_STATUS(CUSTOMER_NAME_IS_TOO_LONG);
    }
    strcpy(customer->name, name);
    RETURN_STATUS(SUCCESS);
}

STATUS get_customer_personal_data_buf_size(const pCustomer customer, size_t* buf_size)
{
    ASSERT(customer != NULL);
    ASSERT(buf_size != NULL);
    int len;
    RETURN_ON_ERROR(snprintf_personal_data(customer, NULL, 0, &len));
    *buf_size = (size_t)len + 1;
    RETURN_STATUS(SUCCESS);
}

STATUS get_customer_personal_data(const pCustomer customer, char* buf, size_t buf_size)
{
    ASSERT(customer != NULL);
    ASSERT(buf != NULL);
    // check buf_size:
    size_t buf_size_required;
    RETURN_ON_ERROR(get_customer_personal_data_buf_size(customer,
        &buf_size_required));
    if (buf_size_required > buf_size) {
        RETURN_STATUS(INSUFFICIENT_BUFFER_SIZE);
    }
    RETURN_STATUS(write_customer_personal_data(customer, buf, buf_size));
}

STATUS get_customer_personal_data_malloc(const pCustomer customer, char** buf)
{
    ASSERT(customer != NULL);
    ASSERT(buf != NULL);

    size_t buf_size_required;
    RETURN_ON_ERROR(get_customer_personal_data_buf_size(customer,
        &buf_size_required));

    char* local_buf;
    RETURN_ON_ERROR(malloc_wrapper((void**)&local_buf, buf_size_required));

    STATUS status = write_customer_personal_data(customer, local_buf,
        buf_size_required);

    if (FAILED(status)) {
        free(local_buf);
        RETURN_STATUS(status);
    }

    *buf = local_buf;
    RETURN_STATUS(SUCCESS);
}
