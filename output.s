.text
.global main
main:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
main_entry:
	li t0, 10
	li t5, 16
	mv a0, t5
	jal ra, malloc
	mv t1, a0
	la t2, func_1
	sd t2, 0(t1)
	sd t0, 8(t1)
	li t3, 3
	mv a0, t3
	addi a1, t1, 8
	ld t6, 0(t1)
	jalr ra, 0(t6)
	mv t4, a0
	mv a0, t4
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

func_1:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
func_1_entry:
	ld t0, 8(a1)
	add t1, a0, t0
	mv a0, t1
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

