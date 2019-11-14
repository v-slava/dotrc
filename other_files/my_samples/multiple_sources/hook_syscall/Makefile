obj-m := hook_syscall.o
#KDIR := /lib/modules/`uname -r`/build
KDIR := /home/user/workspace/linux
PWD := `pwd`
#ccflags-y += -Wall

1default:
	make -C $(KDIR) M=$(PWD) modules ARCH=arm

clean:
	make -C $(KDIR) M=$(PWD) clean
