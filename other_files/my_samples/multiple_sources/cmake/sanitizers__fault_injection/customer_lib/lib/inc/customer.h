#ifndef CUSTOMER_H
#define CUSTOMER_H

#include "status.h"
#include <stddef.h>

typedef struct Customer* pCustomer;

void init_customer_lib(void);
void fini_customer_lib(void);

STATUS create_customer(pCustomer* customer);
void destroy_customer(pCustomer customer);
int get_customer_age(const pCustomer customer);
void set_customer_age(pCustomer customer, int age);
const char* get_customer_name(const pCustomer customer);
STATUS set_customer_name(pCustomer customer, const char* name);

STATUS get_customer_personal_data_buf_size(const pCustomer customer, size_t* buf_size);
STATUS get_customer_personal_data(const pCustomer customer, char* buf, size_t buf_size);
STATUS get_customer_personal_data_malloc(const pCustomer customer, char** buf); // free() required

#endif // #ifndef CUSTOMER_H
