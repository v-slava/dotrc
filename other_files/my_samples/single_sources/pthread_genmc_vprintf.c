#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

/* See: https://github.com/MPI-SWS/genmc */

/* EVAL REGION BEGINS HERE: |* |
 * let g:My_eval_var = "silent wa | MyRunShellCmd clang -g3 -Weverything -pedantic -pthread "
 * \ . expand("%:t") . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
 * \ . expand("%:t") . ".out"
 * EVAL REGION ENDS HERE. */

/* EVAL REGION BEGINS HERE: |* |
 * let g:My_eval_var = "silent wa | MyRunShellCmd genmc -print-error-trace -- "
 * \ . "-DGENMC " . expand("%:t")
 * EVAL REGION ENDS HERE. */

#define NUM_THREADS 2

#define DATA_RACE
/* #define GENMC_CRASH */
/* #define ATOMIC_ACCESS */

#if defined(DATA_RACE)
static int a = 0;
#elif defined(GENMC_CRASH)
static volatile int a = 0; // genmc crashes every 3-rd time...
#elif defined(ATOMIC_ACCESS)
#include <stdatomic.h>
static _Atomic int a = 0;
#endif

static void *start_routine(void *arg)
{
    (void)arg;
    /* no race for this code:
    int b = a + 1;
    (void)b; */
#if defined(DATA_RACE) || defined(GENMC_CRASH)
    ++a;
#elif defined ATOMIC_ACCESS
    atomic_fetch_add_explicit(&a, 1, memory_order_relaxed);
#endif
    return NULL;
}

static void __attribute__((noreturn)) error(const char *format, ...)
{
    // genmc doesn't support vfprintf()
#ifdef GENMC
    (void)format;
#else
    va_list ap;
    va_start(ap, format);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
    vfprintf(stderr, format, ap);
#pragma GCC diagnostic pop
    va_end(ap);
#endif
    exit(1);
}

int main(void)
{
    pthread_t t[NUM_THREADS];
    pthread_attr_t attr;
    int ret;
    void *t_ret;

    ret = pthread_attr_init(&attr);
    if (ret != 0)
        error("pthread_attr_init() returned %d\n", ret);

    for (unsigned i = 0; i < sizeof(t) / sizeof(t[0]); ++i) {
        ret = pthread_create(&t[i], &attr, start_routine, NULL);
        if (ret != 0)
            error("pthread_create(t[%u]) returned %d\n", i, ret);
    }

    ret = pthread_attr_destroy(&attr);
    if (ret != 0)
        error("pthread_attr_destroy() returned %d\n", ret);

    for (unsigned i = 0; i < sizeof(t) / sizeof(t[0]); ++i) {
        ret = pthread_join(t[i], &t_ret);
        if (ret != 0)
            error("pthread_join(t[%u]) returned %d\n", i, ret);
    }

    printf("a = %d\n", a);
    return 0;
}
