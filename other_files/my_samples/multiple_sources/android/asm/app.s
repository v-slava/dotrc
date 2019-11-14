	.arch armv5te
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 0
	.eabi_attribute 18, 4
	.file	"app.c"
	.section	.rodata
	.align	2
.LC0:
	.ascii	"Hello world\000"
	.text
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	ldr	r3, .L3
.LPIC0:
	add	r3, pc, r3
	mov	r0, r3
	bl	puts(PLT)
	mov	r3, #0
	mov	r0, r3
	ldmfd	sp!, {fp, pc}
.L4:
	.align	2
.L3:
	.word	.LC0-(.LPIC0+8)
	.size	main, .-main
	.ident	"GCC: (GNU) 4.7"
	.section	.note.GNU-stack,"",%progbits

