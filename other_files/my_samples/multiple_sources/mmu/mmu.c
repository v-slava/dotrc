#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/unistd.h>
#include <linux/slab.h>

#include <asm/page.h>
#include <linux/mm.h>
#include <linux/pagemap.h>
#include <linux/slab.h>
#include <linux/vmalloc.h>

union TTBCR_reg {
	u32 reg;
	struct {
		u32 T0SZ:3;
		u32 nu1:4;
		u32 EPD0:1;
		u32 IRGN0:2;
		u32 ORGN0:2;
		u32 SH0:2;
		u32 nu2:2;
		u32 T1SZ:3;
		u32 nu3:4;
		u32 EPD1:1;
		u32 IRGN1:2;
		u32 ORGN1:2;
		u32 SH1:2;
		u32 nu4:1;
		u32 EAE:1;
	};
} TTBCR;

union TTBR_reg {
	u64 reg;
	struct {
		u32 regl;
		u32 regh;
	};
	struct {
		u64 BADDR:40;
		u32 nu1:8;
		u32 ASID:8;
		u32 nu2:8;
	};
} TTBR0, TTBR1;

union PTE {

	u64 reg;

	struct {
		u64 addres_no_shifted:40;
	};

	struct {
		u32 is_valid:1;
		u32 type:1;
	};

	struct {
		u32 is_valid:1;
		u32 type:1;
		u32 lowe_block_attributes:10;
		u32 UNK_SBZP:18;
		u64 addres:10;
		u32 UNK__SBZP:12;
		u32 upper_block_attributes:12;
	} block_l1;

	struct {
		u32 is_valid:1;
		u32 type:1;
		u32 lowe_block_attributes:10;
		u32 UNK_SBZP:9;
		u64 addres:19;
		u32 UNK__SBZP:12;
		u32 upper_block_attributes:12;
	} block_l2;

	struct {
		u32 is_valid:1;
		u32 type:1;
		u32 lowe_page_attributes:10;
		u64 addres:28;
		u32 UNK__SBZP:12;
		u32 upper_page_attributes:12;
	} page;

	struct {
		u32 is_valid:1;
		u32 type:1;
		u32 ignored_1:10;
		u64 addres:28;
		u32 UNK__SBZP:12;
		u32 ignore_2:7;
		u32 PXNTable:1;
		u32 XNTable:1;
		u32 APTable:2;
		u32 NSTable:1;
	} table;
};

void test_1l_table_lpae(union PTE* base, u32 start, u32 size);
void test_2l_table_lpae(union PTE* base, u32 start, u32 size);
void test_3l_table_lpae(union PTE* base, u32 start, u32 size);

void print_1l_table_lpae(union PTE* base, u32 start, u32 size);
void print_2l_table_lpae(union PTE* base, u32 start, u32 size);

void print_1l_block_lpae(union PTE* base, u32 start, u32 size);
void print_2l_block_lpae(union PTE* base, u32 start, u32 size);
void print_3l_page_lpae(union PTE* base, u32 start, u32 size);

#define EXECUTE_READ(instruction) \
	asm(instruction : [Rt]"=r" (reg) ::);
//	pr_err(instruction"\n");

void print_mmu_config(void)
{
	unsigned int reg;
	u32 T0size;
	u32 T1size;
	u32 TTBR0_x;
	u32 TTBR1_x;
	u64 base_addres_0;
	u64 base_addres_1;

	pr_err("\n\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");

	pr_err("ID_MMFR0 - indicates support for a VMSA:\n");
	EXECUTE_READ("MRC	p15, 0, %[Rt], c0, c1, 4")
	switch (reg & 0x0F)
	{
		case 0b0000:
			pr_err("Not supported. PMSA support field must be set to 0b0000.");
			break;
		case 0b0001:
			pr_err("Support for IMPLEMENTATION DEFINED VMSA.");
			break;
		case 0b0010:
			pr_err("Support for VMSAv6, with Cache and TLB Type Registers implemented.");
			break;
		case 0b0011:
			pr_err("Support for VMSAv7, with support for remapping and the Access flag. ARMv7-A profile.");
			break;
		case 0b0100:
			pr_err("As for 0b0011, and adds support for the PXN bit in the Short-descriptor translation table format descriptors.");
			break;
		case 0b0101:
			pr_err("As for 0b0100, and adds support for the Long-descriptor translation table format.");
			break;
	}
	pr_err("\n");
	pr_err("ID_PFR1, Processor Feature Register 1, VMSA - gives information about \
the programmers’ model and Security Extensions support.\n");
	EXECUTE_READ("mrc	p15, 0, %[Rt], c0, c1, 1")
	switch ((reg & 0x0F0) >> 4)
	{
		case 0b0000:
			pr_err("Security extensions not implemented.\n");
			break;
		case 0b0001:
			pr_err("Security Extensions implemented.\n\
This includes support for Monitor mode and the SMC instruction.\n");
			break;
		case 0b0010:
			pr_err("As for 0b0001, and adds the ability to set the NSACR.RFR bit.\n");
			break;
	}
	switch ((reg & 0x0F000) >> 12)
	{
		case 0b0000:
			pr_err("Virtualization Extensions not implemented.\n");
			break;
		case 0b0001:
			pr_err("Virtualization Extensions implemented.\n");
			break;
	}
	pr_err("\n");

	pr_err("SCTLR - provides the top level control of the system, including its memory system:\n");
	EXECUTE_READ("mrc	p15, 0, %[Rt], c1, c0, 0")
	if (reg & 0x01)
		pr_err("PL1&0 stage 1 MMU enabled (SCTLR.M = 1)");
	else
		pr_err("PL1&0 stage 1 MMU disabled (SCTLR.M = 0)");
	pr_err("\n");

	pr_err("TTBCR, Translation Table Base Control Register:\n");
	EXECUTE_READ("mrc	p15, 0, %[Rt], c2, c0, 2")
	TTBCR.reg = reg;
	pr_err("TTBCR = 0x%X", TTBCR.reg);

	if (TTBCR.EAE) {
		pr_err("TTBR0, TTBR1 64-bit format is used\n");

		T0size = 2 << (32 - TTBCR.T0SZ);
		T1size = 2 << (32 - TTBCR.T1SZ);

		TTBR0_x = TTBCR.T0SZ > 1 ? 14 : 5;
		TTBR0_x -= TTBCR.T0SZ;
		TTBR1_x = TTBCR.T1SZ > 1 ? 14 : 5;
		TTBR1_x -= TTBCR.T1SZ;

		pr_err("Translation table base addres width\n");
		if (TTBR0_x)  pr_err("TTBR0 x = %d\n",TTBR0_x);
		if (TTBR1_x)  pr_err("TTBR1 x = %d\n",TTBR1_x);



		asm("MRRC p15, 0, %[Rt], %[Rt2], c2"
				: [Rt]"=r" (TTBR0.regl), [Rt2]"=r" (TTBR0.regh)
				::);

		asm("MRRC p15, 1, %[Rt], %[Rt2], c2"
				: [Rt]"=r" (TTBR1.regl), [Rt2]"=r" (TTBR1.regh)
				::);

		pr_err("TTBR0 = 0x%016llX\n",TTBR0.reg);
		pr_err("TTBR1 = 0x%016llX\n",TTBR1.reg);

		base_addres_0 = TTBR0.BADDR & (0xffffffffff << TTBR0_x);
		base_addres_1 = TTBR1.BADDR & (0xffffffffff << TTBR1_x);

		if (TTBCR.T1SZ == 0) {
			if (TTBCR.T0SZ == 0) {
				pr_err("TTBR1 not used");
			} else {
				pr_err("TTBR0 [0x00, 0x%X]; TTBR1 [0x%X - 0xFFFFFFFF]\n",T0size - 1, T0size);
			}
		} else {
			if (TTBCR.T0SZ == 0) {
				pr_err("TTBR0 [0x00, 0x%X]; TTBR1 [0x%X, 0xFFFFFFFF]\n",-1 - T1size, -T1size);

				pr_err("Phis base addres 0 = 0x%llX 0x%llX\n",base_addres_0, base_addres_0 & 0xFFFFF000);
				pr_err("Virt base addres 0 = 0x%p\n\n", phys_to_virt( base_addres_0 ) );
				test_1l_table_lpae( phys_to_virt( base_addres_0 ), 0, -1);//- T1size );

				pr_err("Phis base addres 1 = 0x%llX 0x%llX\n",base_addres_1, base_addres_1 & 0xFFFFF000);
				pr_err("Virt base addres 1 = 0x%p\n\n", phys_to_virt( base_addres_1 ) );
				test_1l_table_lpae( phys_to_virt( base_addres_1 ), 0, -1);//-T1size, T1size );
			} else {
				pr_err("TTBR0 [0x00, 0x%X]; TTBR1 [0x%X, 0xFFFFFFFF]\n",T0size - 1, T1size);
			}
		}
	} else {
		pr_err("TTBR0, TTBR1 32-bit format is used\n");



	}
	pr_err("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\n");
}

void test_1l_table_lpae(union PTE* base, u32 start, u32 size)
{
	union PTE *pte = base;
	const u32 block_size = 0x40000000;
	u64 adr = 0;
	struct page * pages[8];
	void *page_table_adress;

	while (adr < (u64) start) {
		adr += block_size;
		pte++;
	}

	while (adr < (u64) start + size) {
//		pr_err("PTE = 0x%p; *PTE = 0x%llX\n",pte, pte->reg);
		switch (pte->is_valid) {
			case 0:
//				pr_err("ignore\n");
			break;

			case 1:
				switch (pte->type) {
					case 0:
						print_1l_block_lpae(pte, adr, block_size);

					break;

					case 1:
						print_1l_table_lpae(pte, adr, block_size);
						pages[0] = phys_to_page(pte->table.addres << 12);
						page_table_adress = vmap( pages, 1, VM_MAP, PAGE_KERNEL);
						if ( !page_table_adress ) {
							pr_err("vmap error phys adr: 0x%08X !!!\n",pte->table.addres << 12);
							return;
						}
						test_2l_table_lpae(page_table_adress, adr, block_size);
						vunmap(page_table_adress);
					break;
				}
			break;
		}
		adr += block_size;
		pte++;
	}
}

void test_2l_table_lpae(union PTE* base, u32 start, u32 size)
{
	union PTE *pte = base;
	const u32 block_size = 0x200000;
	u64 adr = start;
	struct page * pages[8];
	void *page_table_adress;

	while (adr < (u64) start + size) {
//		pr_err("PTE = 0x%p; *PTE = 0x%llX\n",pte, pte->reg);
		switch (pte->is_valid) {
			case 0:
//				pr_err("ignore\n");
			break;

			case 1:
				switch (pte->type) {
					case 0:
						print_2l_block_lpae(pte, adr, block_size);

					break;

					case 1:
						print_2l_table_lpae(pte, adr, block_size);
						pages[0] = phys_to_page(pte->table.addres << 12);
						page_table_adress = vmap( pages, 1, VM_MAP, PAGE_KERNEL);
						if ( !page_table_adress ) return;
						test_3l_table_lpae(page_table_adress, adr, block_size);
						vunmap(page_table_adress);
					break;
				}
			break;
		}
		adr += block_size;
		pte++;
	}
}

void test_3l_table_lpae(union PTE* base, u32 start, u32 size)
{
	union PTE *pte = base;
	const u32 block_size = 0x1000;
	u64 adr = start;

	while (adr < (u64) start + size) {
//		pr_err("PTE = 0x%p; *PTE = 0x%llX\n",pte, pte->reg);
		switch (pte->is_valid) {
			case 0:
//				pr_err("ignore\n");
			break;

			case 1:
				switch (pte->type) {
					case 0:
//						pr_err("reserved\n");
					break;

					case 1:
						print_3l_page_lpae(pte, adr, block_size);
					break;
				}
			break;
		}
		adr += block_size;
		pte++;
	}
}

void print_1l_block_lpae(union PTE* base, u32 start, u32 size)
{
	pr_err("1 BLOCK:  virt [0x%8X,0x%8X] => phis [0x%8X,0x%8X]\n",
		start, start + size - 1, base->block_l1.addres << 30,
		(base->block_l1.addres << 30) + size - 1);
}

void print_2l_block_lpae(union PTE* base, u32 start, u32 size)
{
	pr_err("2\tBLOCK:  virt [0x%8X,0x%8X] => phis [0x%8X,0x%8X]\n",
		start, start + size - 1, base->block_l2.addres << 21,
		(base->block_l2.addres << 21) + size - 1);
	if (start == 0xc0000000)
	{
		pr_err("PTE = %.16llX\n", base->reg);
		// change AP[2:1] (bits 7:6) from 00 (read-write, only at PL1) to 10 (read-only, only at PL1)
		base->reg |= 0x0000000000000080;
		pr_err("Successfully changed!\n");
	}
}

void print_3l_page_lpae(union PTE* base, u32 start, u32 size)
{
	pr_err("3\t\tPAGE :  virt [0x%8X,0x%8X] => phis [0x%8X,0x%8X]\n",
		start, start + size - 1, base->page.addres << 12,
		(base->page.addres << 12) + size - 1);
}

void print_1l_table_lpae(union PTE* base, u32 start, u32 size)
{
	pr_err("PTE = 0x%p; *PTE = 0x%llX\n",base, base->reg);
}

void print_2l_table_lpae(union PTE* base, u32 start, u32 size)
{
	pr_err("PTE = 0x%p; *PTE = 0x%llX\n",base, base->reg);
}
