.text
.global main
main:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
main_entry:
	li t0, 10
	mv a0, t0
	ret
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

