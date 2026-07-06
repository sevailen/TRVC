.text
.global main
main:
	addi sp, sp, -16
	sd ra, 8(sp)
	sd fp, 0(sp)
	mv fp, sp
main_entry:
	li t4, 8
	mv a0, t4
	jal ra, malloc
	mv t0, a0
	la t1, func_1
	sd t1, 0(t0)
	li t2, 5
	mv a0, t2
	addi a1, t0, 8
	ld t5, 0(t0)
	jalr ra, 0(t5)
	mv t3, a0
	mv a0, t3
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
	li t2, 1
	add t0, a0, t2
	add t1, t0, t0
	mv a0, t1
	mv sp, fp
	ld ra, 8(sp)
	ld fp, 0(sp)
	addi sp, sp, 16
	ret

