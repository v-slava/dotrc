obj-m := mmu_test.o
#KDIR := /home/user/linux
#KDIR := /lib/modules/`uname -r`/build
PWD := `pwd`
ccflags-y += -Wall

mmu_test-y = kmod.o
mmu_test-y += mmu.o

default:
	make -C $(KDIR) M=$(PWD) modules ARCH=arm

clean:
	make -C $(KDIR) M=$(PWD) clean
