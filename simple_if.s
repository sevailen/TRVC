.text
.global main
main:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
main_entry:
	li t0, 1
	li t1, 2
	j L_1
L_1:
	add t2, t0, t1
	mv t3, t2
	j L_3
L_2:
	sub t4, t0, t1
	mv t3, t4
	j L_3
L_3:
	mv a0, t3
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

