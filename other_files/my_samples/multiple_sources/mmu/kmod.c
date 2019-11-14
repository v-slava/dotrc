#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/unistd.h>
#include <linux/slab.h>

extern void print_mmu_config(void);

static int mmu_test_init(void)
{
    pr_err("Mmu test module init start\n");
    print_mmu_config();
    pr_err("Mmu test module init finish\n");
    return 0;
}

static void mmu_test_exit(void)
{
    pr_err("Mmu test module exit start\n");
    pr_err("Mmu test module exit finish\n");
}

module_init(mmu_test_init);
module_exit(mmu_test_exit);

MODULE_LICENSE("GPL");
